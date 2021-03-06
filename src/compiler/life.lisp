;;; The lifetime analysis phase in the compiler.

(in-package "C")

#[ Lifetime Analysis

    Do flow analysis to find the set of TNs whose lifetimes overlap with
    the lifetimes of each TN being packed.  Annotate call VOPs with the TNs
    that need to be saved.

Phase position: 18/23 (back)

Presence: required

Files: life

Entry functions: `lifetime-analyze'

Call sequences:

    native-compile-component
      lifetime-analyze
        lifetime-pre-pass
          find-local-references
            add-global-conflict
          init-global-conflict-kind
            convert-to-global
              add-global-conflict
          clear-lifetime-info
          split-ir2-blocks
          coalesce-more-ltn-numbers
        setup-environment-live-conflicts
          setup-environment-tn-conflicts
        lifetime-flow-analysis
          reset-current-conflict
          propagate-live-tns
        lifetime-post-pass
          conflict-analyze-1-block
            compute-initial-conflicts
            do-save-p-stuff
              compute-save-set
              convert-to-environment-tn
            ensure-results-live
            scan-vop-refs
              frob-more-tns
        merge-alias-conflicts
          ensure-global-tn
          change-global-conflicts-tn
          merge-alias-block-conflicts
          change-tn-ref-tn


This phase is a preliminary to Pack.  It involves three passes:
 * A pre-pass that computes the DEF and USE sets for live TN analysis, while
   also assigning local TN numbers, splitting blocks if necessary.
       FIX But not really...
 * A flow analysis pass that does backward flow analysis on the
   component to find the live TNs at each block boundary.
 * A post-pass that finds the conflict set for each TN.

    Exploit the fact that a single VOP can only exhaust LTN numbers when there are
    large more operands.  Since more operand reference cannot be interleaved with
    temporary reference, the references all effectively occur at the same time.
    This means that we can assign all the more args and all the more results the
    same LTN number and the same lifetime info.


== Flow analysis ==

It seems we could use the global-conflicts structures during compute the
inter-block lifetime information.  The pre-pass creates all the
global-conflicts for blocks that global TNs are referenced in.  The flow
analysis pass just adds always-live global-conflicts for the other blocks the
TNs are live in.  In addition to possibly being more efficient than SSets, this
would directly result in the desired global-conflicts information, rather that
having to create it from another representation.

The DFO sorted per-TN global-conflicts thread suggests some kind of algorithm
based on the manipulation of the sets of blocks each TN is live in (which is
what we really want), rather than the set of TNs live in each block.

If we sorted the per-TN global-conflicts in reverse DFO (which is just as good
for determining conflicts between TNs), then it seems we could scan though the
conflicts simultaneously with our flow-analysis scan through the blocks.

The flow analysis step is the following:
    If a TN is always-live or read-before-written in a successor block, then we
    make it always-live in the current block unless there are already
    global-conflicts recorded for that TN in this block.

The iteration terminates when we don't add any new global-conflicts during a
pass.

We may also want to promote TNs only read within a block to always-live when
the TN is live in a successor.  This should be easy enough as long as the
global-conflicts structure contains this kind of info.

The critical operation here is determining whether a given global TN has global
conflicts in a given block.  Note that since we scan the blocks in DFO, and the
global-conflicts are sorted in DFO, if we give each global TN a pointer to the
global-conflicts for the last block we checked the TN was in, then we can
guarantee that the global-conflicts we are looking for are always at or after
that pointer.  If we need to insert a new structure, then the pointer will help
us rapidly find the place to do the insertion.]


== Conflict detection ==

    XXX Environment, :more TNs.

    FIX "phase"s in life conflict with compiler "phase"s

This phase makes use of the results of lifetime analysis to find the set of TNs
that have lifetimes overlapping with those of each TN.  We also annotate call
VOPs with information about the live TNs so that code generation knows which
registers need to be saved.

The basic action is a backward scan of each block, looking at each TN-Ref and
maintaining a set of the currently live TNs.  When we see a read, we check if
the TN is in the live set.  If not, we:
 * Add the TN to the conflict set for every currently live TN,
 * Union the set of currently live TNs with the conflict set for the TN, and
 * Add the TN to the set of live TNs.

When we see a write for a live TN, we just remove it from the live set.  If we
see a write to a dead TN, then we update the conflicts sets as for a read, but
don't add the TN to the live set.  We have to do this so that the bogus write
doesn't clobber anything.

    We don't consider always-live TNs at all in this process, since the conflict
    of always-live TNs with other TNs in the block is implicit in the
    global-conflicts structures.

    Before we do the scan on a block, we go through the global-conflicts structures
    of TNs that change liveness in the block, assigning the recorded LTN number to
    the TN's LTN number for the duration of processing of that block.


Efficiently computing and representing this information calls for some
cleverness.  It would be prohibitively expensive to represent the full conflict
set for every TN with sparse sets, as is done at the block-level.  Although it
wouldn't cause non-linear behavior, it would require a complex linked structure
containing tens of elements to be created for every TN.  Fortunately we can
improve on this if we take into account the fact that most TNs are "local" TNs:
TNs which have all their uses in one block.

First, many global TNs will be either live or dead for the entire duration of a
given block.  We can represent the conflict between global TNs live throughout
the block and TNs local to the block by storing the set of always-live global
TNs in the block.  This reduces the number of global TNs that must be
represented in the conflicts for local TNs.

Second, we can represent conflicts within a block using bit-vectors.  Each TN
that changes liveness within a block is assigned a local TN number.  Local
conflicts are represented using a fixed-size bit-vector of 64 elements or so
which has a 1 for the local TN number of every TN live at that time.  The block
has a simple-vector which maps from local TN numbers to TNs.  Fixed-size
vectors reduce the hassle of doing allocations and allow operations to be
open-coded in a maximally tense fashion.

We can represent the conflicts for a local TN by a single bit-vector indexed by
the local TN numbers for that block, but in the global TN case, we need to be
able to represent conflicts with arbitrary TNs.  We could use a list-like
sparse set representation, but then we would have to either special-case global
TNs by using the sparse representation within the block, or convert the local
conflicts bit-vector to the sparse representation at the block end.  Instead,
we give each global TN a list of the local conflicts bit-vectors for each block
that the TN is live in.  If the TN is always-live in a block, then we record
that fact instead.  This gives us a major reduction in the amount of work we
have to do in lifetime analysis at the cost of some increase in the time to
iterate over the set during Pack.

Since we build the lists of local conflict vectors a block at a time, the
blocks in the lists for each TN will be sorted by the block number.  The
structure also contains the local TN number for the TN in that block.  These
features allow pack to efficiently determine whether two arbitrary TNs
conflict.  You just scan the lists in order, skipping blocks that are in only
one list by using the block numbers.  When we find a block that both TNs are
live in, we just check the local TN number of one TN in the local conflicts
vector of the other.

In order to do these optimizations, we must do a pre-pass that finds the
always-live TNs and breaks blocks up into small enough pieces so that we don't
run out of local TN numbers.  If we can make a block arbitrarily small, then we
can guarantee that an arbitrarily small number of TNs change liveness within
the block.  We must be prepared to make the arguments to unbounded arg count
VOPs (such as function call) always-live even when they really aren't.  This is
enabled by a panic mode in the block splitter: if we discover that the block
only contains one VOP and there are still too many TNs that aren't always-live,
then we promote the arguments (which we'd better be able to do...).

This is done during the pre-scan in lifetime analysis.  We can do this because
all TNs that change liveness within a block can be found by examining that
block: the flow analysis only adds always-live TNs.


When we are doing the conflict detection pass, we set the LTN number of global
TNs.  We can easily detect global TNs that have not been locally mapped because
this slot is initially null for global TNs and we null it out after processing
each block.  We assign all Always-Live TNs to the same local number so that we
don't need to treat references to them specially when making the scan.

We also annotate call VOPs that do register saving with the TNs that are live
during the call, and thus would need to be saved if they are packed in
registers.

We adjust the costs for TNs that need to be saved so that TNs costing more to
save and restore than to reference get packed on the stack.  We would also like
more often saved TNs to get higher costs so that they are packed in more
savable locations.
]#


;;;; Utilities.

;;; Add-Global-Conflict  --  Internal
;;;
;;; Link in a global-conflicts structure for TN in Block with Number as the
;;; LTN number.  The conflict is inserted in the per-TN Global-Conflicts
;;; thread after the TN's Current-Conflict.  We change the Current-Conflict
;;; to point to the new conflict.  Since we scan the blocks in reverse DFO,
;;; this list is automatically built in order.  We have to actually scan
;;; the current Global-TNs for the block in order to keep that thread
;;; sorted.
;;;
(defun add-global-conflict (kind tn block number)
  (declare (type (member :read :write :read-only :live) kind)
	   (type tn tn) (type ir2-block block)
	   (type (or local-tn-number null) number))
  (let ((new (make-global-conflicts kind tn block number)))
    (let ((last (tn-current-conflict tn)))
      (if last
	  (shiftf (global-conflicts-tn-next new)
		  (global-conflicts-tn-next last)
		  new)
	  (shiftf (global-conflicts-tn-next new)
		  (tn-global-conflicts tn)
		  new)))
    (setf (tn-current-conflict tn) new)

    (insert-block-global-conflict new block))
  (undefined-value))

;;; INSERT-BLOCK-GLOBAL-CONFLICT  --  Internal
;;;
;;; Do the actual insertion of the conflict New into Block's global
;;; conflicts.
;;;
(defun insert-block-global-conflict (new block)
  (let ((global-num (tn-number (global-conflicts-tn new))))
    (do ((prev nil conf)
	 (conf (ir2-block-global-tns block)
	       (global-conflicts-next conf)))
	((or (null conf)
	     (> (tn-number (global-conflicts-tn conf)) global-num))
	 (if prev
	     (setf (global-conflicts-next prev) new)
	     (setf (ir2-block-global-tns block) new))
	 (setf (global-conflicts-next new) conf))))
  (undefined-value))

;;; Reset-Current-Conflict  --  Internal
;;;
;;; Reset the Current-Conflict slot in all packed TNs to point to the head
;;; of the Global-Conflicts thread.
;;;
(defun reset-current-conflict (component)
  (do-packed-tns (tn component)
    (setf (tn-current-conflict tn) (tn-global-conflicts tn))))


;;;; Pre-pass.

;;; Convert-To-Global  --  Internal
;;;
;;; Convert TN (currently local) to be a global TN, since we discovered
;;; that it is referenced in more than one block.  We just add a
;;; global-conflicts structure with a kind derived from the Kill and Live
;;; sets.
;;;
(defun convert-to-global (tn)
  (declare (type tn tn))
  (let ((block (tn-local tn))
	(num (tn-local-number tn)))
    (add-global-conflict
     (if (zerop (sbit (ir2-block-written block) num))
	 :read-only
	 (if (zerop (sbit (ir2-block-live-out block) num))
	     :write
	     :read))
     tn block num))
  (undefined-value))

;;; Find-Local-References  --  Internal
;;;
;;; Scan all references to packed TNs in block.  We assign LTN numbers to
;;; each referenced TN, and also build the Kill and Live sets that
;;; summarize the references to each TN for purposes of lifetime analysis.
;;;
;;; It is possible that we will run out of LTN numbers.  If this happens,
;;; then we return the VOP that we were processing at the time we ran out,
;;; otherwise we return NIL.
;;;
;;; If a TN is referenced in more than one block, then we must represent
;;; references using Global-Conflicts structures.  When we first see a TN,
;;; we assume it will be local.  If we see a reference later on in a
;;; different block, then we go back and fix the TN to global.
;;;
;;; We must globalize TNs that have a block other than the current one in
;;; their Local slot and have no Global-Conflicts.  The latter condition is
;;; necessary because we always set Local and Local-Number when we process
;;; a reference to a TN, even when the TN is already known to be global.
;;;
;;; When we see reference to global TNs during the scan, we add the
;;; global-conflict as :Read-Only, since we don't know the corrent kind
;;; until we are done scanning the block.
;;;
(defun find-local-references (block)
  (declare (type ir2-block block))
  (let ((kill (ir2-block-written block))
	(live (ir2-block-live-out block))
	(tns (ir2-block-local-tns block)))
    (let ((ltn-num (ir2-block-local-tn-count block)))
      (do ((vop (ir2-block-last-vop block)
		(vop-prev vop)))
	  ((null vop))
	(do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
	    ((null ref))
	  (let* ((tn (tn-ref-tn ref))
		 (local (tn-local tn))
		 (kind (tn-kind tn)))
	    (unless (member kind '(:component :environment :constant))
	      (unless (eq local block)
		(when (= ltn-num local-tn-limit)
		  (return-from find-local-references vop))
		(when local
		  (unless (tn-global-conflicts tn)
		    (convert-to-global tn))
		  (add-global-conflict :read-only tn block ltn-num))

		(setf (tn-local tn) block)
		(setf (tn-local-number tn) ltn-num)
		(setf (svref tns ltn-num) tn)
		(incf ltn-num))

	      (let ((num (tn-local-number tn)))
		(if (tn-ref-write-p ref)
		    (setf (sbit kill num) 1  (sbit live num) 0)
		    (setf (sbit live num) 1)))))))

      (setf (ir2-block-local-tn-count block) ltn-num)))
  nil)

;;; Init-Global-Conflict-Kind   --  Internal
;;;
;;; Finish up the global conflicts for TNs referenced in Block according to
;;; the local Kill and Live sets.
;;;
;;; We set the kind for TNs already in the global-TNs.  If not written at
;;; all, then is :Read-Only, the default.  Must have been referenced
;;; somehow, or we wouldn't have conflicts for it.
;;;
;;; We also iterate over all the local TNs, looking for TNs local to this
;;; block that are still live at the block beginning, and thus must be
;;; global.  This case is only important when a TN is read in a block but
;;; not written in any other, since otherwise the write would promote the
;;; TN to global.  But this does happen with various passing-location TNs
;;; that are magically written.  This also serves to propagate the lives of
;;; erroneously uninitialized TNs so that consistency checks can detect
;;; them.
;;;
(defun init-global-conflict-kind (block)
  (declare (type ir2-block block))
  (let ((live (ir2-block-live-out block)))
    (let ((kill (ir2-block-written block)))
      (do ((conf (ir2-block-global-tns block)
		 (global-conflicts-next conf)))
	  ((null conf))
	(let ((num (global-conflicts-number conf)))
	  (unless (zerop (sbit kill num))
	    (setf (global-conflicts-kind conf)
		  (if (zerop (sbit live num))
		      :write
		      :read))))))

    (let ((ltns (ir2-block-local-tns block)))
      (dotimes (i (ir2-block-local-tn-count block))
	(let ((tn (svref ltns i)))
	  (unless (or (eq tn :more)
		      (tn-global-conflicts tn)
		      (zerop (sbit live i)))
	    (convert-to-global tn))))))

  (undefined-value))

(defevent split-ir2-block "Split an IR2 block to meet Local-TN-Limit.")

;;; Split-IR2-Blocks  --  Internal
;;;
;;; Move the code after the VOP Lose in 2block into its own block.  The
;;; block is linked into the emit order following 2block.  Number is the
;;; block number assigned to the new block.  We return the new block.
;;;
(defun split-ir2-blocks (2block lose number)
  (declare (type ir2-block 2block) (type vop lose)
	   (type unsigned-byte number))
  (event split-ir2-block (vop-node lose))
  (let ((new (make-ir2-block (ir2-block-block 2block)))
	(new-start (vop-next lose)))
    (setf (ir2-block-number new) number)
    (add-to-emit-order new 2block)

    (do ((vop new-start (vop-next vop)))
	((null vop))
      (setf (vop-block vop) new))

    (setf (ir2-block-start-vop new) new-start)
    (shiftf (ir2-block-last-vop new) (ir2-block-last-vop 2block) lose)

    (setf (vop-next lose) nil)
    (setf (vop-prev new-start) nil)

    new))

;;; Clear-Lifetime-Info  --  Internal
;;;
;;; Clear the global and local conflict info in Block so that we can
;;; recompute it without any old cruft being retained.  It is assumed that
;;; all LTN numbers are in use.
;;;
;;; First we delete all the global conflicts.  The conflict we are deleting
;;; must be the last in the TN's global-conflicts, but we must scan for it
;;; in order to find the previous conflict.
;;;
;;; Next, we scan the local TNs, nulling out the Local slot in all TNs with
;;; no global conflicts.  This allows these TNs to be treated as local when
;;; we scan the block again.
;;;
;;; If there are conflicts, then we set Local to one of the conflicting
;;; blocks.  This ensures that Local doesn't hold over Block as its value,
;;; causing the subsequent reanalysis to think that the TN has already been
;;; seen in that block.
;;;
;;; This function must not be called on blocks that have :More TNs.
;;;
(defun clear-lifetime-info (block)
  (declare (type ir2-block block))
  (setf (ir2-block-local-tn-count block) 0)

  (do ((conf (ir2-block-global-tns block)
	     (global-conflicts-next conf)))
      ((null conf)
       (setf (ir2-block-global-tns block) nil))
    (let ((tn (global-conflicts-tn conf)))
      (assert (eq (tn-current-conflict tn) conf))
      (assert (null (global-conflicts-tn-next conf)))
      (do ((current (tn-global-conflicts tn)
		    (global-conflicts-tn-next current))
	   (prev nil current))
	  ((eq current conf)
	   (if prev
	       (setf (global-conflicts-tn-next prev) nil)
	       (setf (tn-global-conflicts tn) nil))
	   (setf (tn-current-conflict tn) prev)))))

  (fill (ir2-block-written block) 0)
  (let ((ltns (ir2-block-local-tns block)))
    (dotimes (i local-tn-limit)
      (let ((tn (svref ltns i)))
	(assert (not (eq tn :more)))
	(let ((conf (tn-global-conflicts tn)))
	  (setf (tn-local tn)
		(if conf
		    (global-conflicts-block conf)
		    nil))))))

  (undefined-value))

;;; Coalesce-More-LTN-Numbers  --  Internal
;;;
;;; This provides a panic mode for assigning LTN numbers when there is a
;;; VOP with so many more operands that they can't all be assigned distinct
;;; numbers.  When this happens, we recover by assigning all the more
;;; operands the same LTN number.  We can get away with this, since all
;;; more args (and results) are referenced simultaneously as far as
;;; conflict analysis is concerned.
;;;
;;; Block is the IR2-Block that the more VOP is at the end of.  Ops is the
;;; full argument or result TN-Ref list.  Fixed is the types of the fixed
;;; operands (used only to skip those operands.)
;;;
;;; What we do is grab a LTN number, then make a :Read-Only global conflict
;;; for each more operand TN.  We require that there be no existing global
;;; conflict in Block for any of the operands.  Since conflicts must be
;;; cleared before the first call, this only prohibits the same TN being
;;; used both as a more operand and as any other operand to the same VOP.
;;;
;;; We don't have to worry about getting the correct conflict kind, since
;;; Init-Global-Conflict-Kind will fix things up.  Similarly,
;;; FIND-LOCAL-REFERENCES will set the local conflict bit corresponding to
;;; this call.
;;;
;;; We also set the Local and Local-Number slots in each TN.  It is
;;; possible that there are no operands in any given call to this function,
;;; but there had better be either some more args or more results.
;;;
(defun coalesce-more-ltn-numbers (block ops fixed)
  (declare (type ir2-block block) (type (or tn-ref null) ops) (list fixed))
  (let ((num (ir2-block-local-tn-count block)))
    (assert (< num local-tn-limit))
    (incf (ir2-block-local-tn-count block))
    (setf (svref (ir2-block-local-tns block) num) :more)

    (do ((op (do ((op ops (tn-ref-across op))
		  (i 0 (1+ i)))
		 ((= i (length fixed)) op)
	       (declare (type index i)))
	     (tn-ref-across op)))
	((null op))
      (let ((tn (tn-ref-tn op)))
	(assert
	  (flet ((frob (refs)
		   (do ((ref refs (tn-ref-next ref)))
		       ((null ref) t)
		     (when (and (eq (vop-block (tn-ref-vop ref)) block)
				(not (eq ref op)))
		       (return nil)))))
	    (and (frob (tn-reads tn)) (frob (tn-writes tn))))
	  () "More operand ~S used more than once in its VOP." op)
	(assert (not (find-in #'global-conflicts-next tn
			      (ir2-block-global-tns block)
			      :key #'global-conflicts-tn)))

	(add-global-conflict :read-only tn block num)
	(setf (tn-local tn) block)
	(setf (tn-local-number tn) num))))
  (undefined-value))

(defevent coalesce-more-ltn-numbers
  "Coalesced LTN numbers for a more operand to meet Local-TN-Limit.")

;;; Lifetime-Pre-Pass  --  Internal
;;;
;;; Loop over the blocks in Component, assigning LTN numbers and recording
;;; TN birth and death.  The only interesting action is when we run out of
;;; local TN numbers while finding local references.
;;;
;;; If we run out of LTN numbers while processing a VOP within the block,
;;; then we just split off the VOPs we have successfully processed into
;;; their own block.
;;;
;;; If we run out of LTN numbers while processing the first VOP (the last
;;; in the block), then it must be the case that this VOP has large more
;;; operands.  We split the VOP into its own block, and then call
;;; `coalesce-more-ltn-numbers' to assign all the more args/results the
;;; same LTN number(s).
;;;
;;; In either case, we clear the lifetime information that we computed so
;;; far, recomputing it after taking corrective action.
;;;
;;; Whenever we split a block, we finish the pre-pass on the split-off
;;; block by doing `find-local-references' and `init-global-conflict-kind'.
;;; This can't run out of LTN numbers.
;;;
(defun lifetime-pre-pass (component)
  (declare (type component component))
  (let ((counter -1))
    (declare (type fixnum counter))
    (do-blocks-backwards (block component)
      (let ((2block (block-info block)))
	(do ((lose (find-local-references 2block)
		   (find-local-references 2block))
	     (last-lose nil lose)
	     (coalesced nil))
	    ((not lose)
	     (init-global-conflict-kind 2block)
	     (setf (ir2-block-number 2block) (incf counter)))

	  (clear-lifetime-info 2block)

	  (cond
	   ((vop-next lose)
	    (assert (not (eq last-lose lose)))
	    (let ((new (split-ir2-blocks 2block lose (incf counter))))
	      (assert (not (find-local-references new)))
	      (init-global-conflict-kind new)))
	   (t
	    (assert (not (eq lose coalesced)))
	    (setq coalesced lose)
	    (event coalesce-more-ltn-numbers (vop-node lose))
	    (let ((info (vop-info lose))
		  (new (if (vop-prev lose)
			   (split-ir2-blocks 2block (vop-prev lose)
					     (incf counter))
			   2block)))
	      (coalesce-more-ltn-numbers new (vop-args lose)
					 (vop-info-arg-types info))
	      (coalesce-more-ltn-numbers new (vop-results lose)
					 (vop-info-result-types info))
	      (let ((lose (find-local-references new)))
		(assert (not lose)))
	      (init-global-conflict-kind new))))))))

  (undefined-value))


;;;; Environment TN stuff.

;;; SETUP-ENVIRONMENT-TN-CONFLICT  --  Internal
;;;
;;; Add a :LIVE global conflict for TN in 2block if there is none present.
;;; If Debug-P is false (a :ENVIRONMENT TN), then modify any existing
;;; conflict to be :LIVE.
;;;
(defun setup-environment-tn-conflict (tn 2block debug-p)
  (declare (type tn tn) (type ir2-block 2block))
  (let ((block-num (ir2-block-number 2block)))
    (do ((conf (tn-current-conflict tn) (global-conflicts-tn-next conf))
	 (prev nil conf))
	((or (null conf)
	     (> (ir2-block-number (global-conflicts-block conf)) block-num))
	 (setf (tn-current-conflict tn) prev)
	 (add-global-conflict :live tn 2block nil))
      (when (eq (global-conflicts-block conf) 2block)
	(unless (or debug-p
		    (eq (global-conflicts-kind conf) :live))
	  (setf (global-conflicts-kind conf) :live)
	  (setf (svref (ir2-block-local-tns 2block)
		       (global-conflicts-number conf))
		nil)
	  (setf (global-conflicts-number conf) nil))
	(setf (tn-current-conflict tn) conf)
	(return))))
  (undefined-value))

;;; SETUP-ENVIRONMENT-TN-CONFLICTS  --  Internal
;;;
;;; Iterate over all the blocks in Env, setting up :LIVE conflicts for TN.
;;; We make the TN global if it isn't already.  The TN must have at least
;;; one reference.
;;;
(defun setup-environment-tn-conflicts (component tn env debug-p)
  (declare (type component component) (type tn tn) (type environment env))
  (when (and debug-p
	     (not (tn-global-conflicts tn))
	     (tn-local tn))
    (convert-to-global tn))
  (setf (tn-current-conflict tn) (tn-global-conflicts tn))
  (do-blocks-backwards (block component)
    (when (eq (block-environment block) env)
      (let* ((2block (block-info block))
	     (last (do ((b (ir2-block-next 2block) (ir2-block-next b))
			(prev 2block b))
		       ((not (eq (ir2-block-block b) block))
			prev))))
	(do ((b last (ir2-block-prev b)))
	    ((not (eq (ir2-block-block b) block)))
	  (setup-environment-tn-conflict tn b debug-p)))))
  (undefined-value))

;;; SETUP-ENVIRONMENT-LIVE-CONFLICTS  --  Internal
;;;
;;; Iterate over all the environment TNs, adding always-live conflicts as
;;; appropriate.
;;;
(defun setup-environment-live-conflicts (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (let* ((env (lambda-environment fun))
	   (2env (environment-info env)))
      (dolist (tn (ir2-environment-live-tns 2env))
	(setup-environment-tn-conflicts component tn env nil))
      (dolist (tn (ir2-environment-debug-live-tns 2env))
	(setup-environment-tn-conflicts component tn env t))))
  (undefined-value))

;;; Convert-To-Environment-TN  --  Internal
;;;
;;; Convert a :NORMAL or :DEBUG-ENVIRONMENT TN to an :ENVIRONMENT TN.  This
;;; requires adding :LIVE conflicts to all blocks in TN-ENV.
;;;
(defun convert-to-environment-tn (tn tn-env)
  (declare (type tn tn) (type environment tn-env))
  (assert (member (tn-kind tn) '(:normal :debug-environment)))
  (when (eq (tn-kind tn) :debug-environment)
    (assert (eq (tn-environment tn) tn-env))
    (let ((2env (environment-info tn-env)))
      (setf (ir2-environment-debug-live-tns 2env)
	    (delete tn (ir2-environment-debug-live-tns 2env)))))
  (setup-environment-tn-conflicts *compile-component* tn tn-env nil)
  (setf (tn-local tn) nil)
  (setf (tn-local-number tn) nil)
  (setf (tn-kind tn) :environment)
  (setf (tn-environment tn) tn-env)
  (push tn (ir2-environment-live-tns (environment-info tn-env)))
  (undefined-value))


;;;; Flow analysis.

;;; Propagate-Live-TNs  --  Internal
;;;
;;; For each Global-TN in Block2 that is :Live, :Read or :Read-Only, ensure
;;; that there is a corresponding Global-Conflict in Block1.  If there is
;;; none, make a :Live Global-Conflict.  If there is a :Read-Only conflict,
;;; promote it to :Live.
;;;
;;; If we added a new conflict, return true, otherwise return false.  We
;;; don't need to return true when we promote a :Read-Only conflict, since
;;; it doesn't reveal any new information to predecessors of Block1.
;;;
;;; We use the Tn-Current-Conflict to walk through the global conflicts.
;;; Since the global conflicts for a TN are ordered by block, we can be
;;; sure that the Current-Conflict always points at or before the block
;;; that we are looking at.  This allows us to quickly determine if there
;;; is a global conflict for a given TN in Block1.
;;;
;;; When we scan down the conflicts, we know that there must be at least
;;; one conflict for TN, since we got our hands on TN by picking it out of
;;; a conflict in Block2.
;;;
;;; We leave the Current-Conflict pointing to the conflict for Block1.  The
;;; Current-Conflict must be initialized to the head of the
;;; Global-Conflicts for the TN between each flow analysis iteration.
;;;
(defun propagate-live-tns (block1 block2)
  (declare (type ir2-block block1 block2))
  (let ((live-in (ir2-block-live-in block1))
	(did-something nil))
    (do ((conf2 (ir2-block-global-tns block2)
		(global-conflicts-next conf2)))
	((null conf2))
      (ecase (global-conflicts-kind conf2)
	((:live :read :read-only)
	 (let* ((tn (global-conflicts-tn conf2))
		(tn-conflicts (tn-current-conflict tn))
		(number1 (ir2-block-number block1)))
	   (assert tn-conflicts)
	   (do ((current tn-conflicts (global-conflicts-tn-next current))
		(prev nil current))
	       ((or (null current)
		    (> (ir2-block-number (global-conflicts-block current))
		       number1))
		(setf (tn-current-conflict tn) prev)
		(add-global-conflict :live tn block1 nil)
		(setq did-something t))
	     (when (eq (global-conflicts-block current) block1)
	       (case (global-conflicts-kind current)
		 (:live)
		 (:read-only
		  (setf (global-conflicts-kind current) :live)
		  (setf (svref (ir2-block-local-tns block1)
			       (global-conflicts-number current))
			nil)
		  (setf (global-conflicts-number current) nil)
		  (setf (tn-current-conflict tn) current))
		 (t
		  (setf (sbit live-in (global-conflicts-number current)) 1)))
	       (return)))))
	(:write)))
    did-something))

;;; Lifetime-Flow-Analysis  --  Internal
;;;
;;; Do backward global flow analysis to find all TNs live at each block
;;; boundary.
;;;
(defun lifetime-flow-analysis (component)
  (loop
    (reset-current-conflict component)
    (let ((did-something nil))
      (do-blocks-backwards (block component)
	(let* ((2block (block-info block))
	       (last (do ((b (ir2-block-next 2block) (ir2-block-next b))
			  (prev 2block b))
			 ((not (eq (ir2-block-block b) block))
			  prev))))

	  (dolist (b (block-succ block))
	    (when (and (block-start b)
		       (propagate-live-tns last (block-info b)))
	      (setq did-something t)))

	  (do ((b (ir2-block-prev last) (ir2-block-prev b))
	       (prev last b))
	      ((not (eq (ir2-block-block b) block)))
	    (when (propagate-live-tns b prev)
	      (setq did-something t)))))

      (unless did-something (return))))

  (undefined-value))


;;;; Post-pass.

;;; Note-Conflicts  --  Internal
;;;
;;; Note that TN conflicts with all current live TNs.  Num is TN's LTN
;;; number.  We bit-ior Live-Bits with TN's Local-Conflicts, and set TN's
;;; number in the conflicts of all TNs in Live-List.
;;;
(defun note-conflicts (live-bits live-list tn num)
  (declare (type tn tn) (type (or tn null) live-list)
	   (type local-tn-bit-vector live-bits)
	   (type local-tn-number num))
  (let ((lconf (tn-local-conflicts tn)))
    (bit-ior live-bits lconf lconf))
  (do ((live live-list (tn-next* live)))
      ((null live))
    (setf (sbit (tn-local-conflicts live) num) 1))
  (undefined-value))

;;; Compute-Save-Set  --  Internal
;;;
;;; Compute a bit vector of the TNs live after VOP that aren't results.
;;;
(defun compute-save-set (vop live-bits)
  (declare (type vop vop) (type local-tn-bit-vector live-bits))
  (let ((live (bit-vector-copy live-bits)))
    (do ((r (vop-results vop) (tn-ref-across r)))
	((null r))
      (let ((tn (tn-ref-tn r)))
	(ecase (tn-kind tn)
	  ((:normal :debug-environment)
	   (setf (sbit live (tn-local-number tn)) 0))
	  (:environment :component))))
    live))

;;; SAVED-AFTER-READ  --  Internal
;;;
;;; Used to determine whether a :DEBUG-ENVIRONMENT TN should be considered
;;; live at block end.  We return true if a VOP with non-null SAVE-P
;;; appears before the first read of TN (hence is seen first in our
;;; backward scan.)
;;;
(defun saved-after-read (tn block)
  (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
      ((null vop) t)
    (when (vop-info-save-p (vop-info vop)) (return t))
    (when (find-in #'tn-ref-across tn (vop-args vop) :key #'tn-ref-tn)
      (return nil))))

;;; MAKE-DEBUG-ENVIRONMENT-TNS-LIVE  --  Internal
;;;
;;; If the block has no successors, or its successor is the component tail,
;;; then all :DEBUG-ENVIRONMENT TNs are always added, regardless of whether
;;; they appeared to be live.  This ensures that these TNs are considered
;;; to be live throughout blocks that read them, but don't have any
;;; interesting successors (such as a return or tail call.)  In this case,
;;; we set the corresponding bit in LIVE-IN as well.
;;;
(defun make-debug-environment-tns-live (block live-bits live-list)
  (let* ((1block (ir2-block-block block))
	 (live-in (ir2-block-live-in block))
	 (succ (block-succ 1block))
	 (next (ir2-block-next block)))
    (when (and next
	       (not (eq (ir2-block-block next) 1block))
	       (or (null succ)
		   (eq (first succ)
		       (component-tail (block-component 1block)))))
      (do ((conf (ir2-block-global-tns block)
		 (global-conflicts-next conf)))
	  ((null conf))
	(let* ((tn (global-conflicts-tn conf))
	       (num (global-conflicts-number conf)))
	  (when (and num (zerop (sbit live-bits num))
		     (eq (tn-kind tn) :debug-environment)
		     (eq (tn-environment tn) (block-environment 1block))
		     (saved-after-read tn block))
	    (note-conflicts live-bits live-list tn num)
	    (setf (sbit live-bits num) 1)
	    (push-in tn-next* tn live-list)
	    (setf (sbit live-in num) 1))))))

  (values live-bits live-list))

;;; Compute-Initial-Conflicts  --  Internal
;;;
;;; Return as values, a LTN bit-vector and a list (threaded by TN-Next*)
;;; representing the TNs live at the end of Block (exclusive of :Live TNs).
;;;
;;; We iterate over the TNs in the global conflicts that are live at the
;;; block end, setting up the TN-Local-Conflicts and TN-Local-Number, and
;;; adding the TN to the live list.
;;;
;;; If a :MORE result is not live, we effectively fake a read to it.  This
;;; is part of the action described in ENSURE-RESULTS-LIVE.
;;;
;;; At the end, we call MAKE-DEBUG-ENVIRONEMNT-TNS-LIVE to make debug
;;; environment TNs appear live when appropriate, even when they aren't.
;;;
;;; ### Note: we alias the global-conflicts-conflicts here as the
;;; tn-local-conflicts.
;;;
(defun compute-initial-conflicts (block)
  (declare (type ir2-block block))
  (let* ((live-in (ir2-block-live-in block))
	 (ltns (ir2-block-local-tns block))
	 (live-bits (bit-vector-copy live-in))
	 (live-list nil))

    (do ((conf (ir2-block-global-tns block)
	       (global-conflicts-next conf)))
	((null conf))
      (let ((bits (global-conflicts-conflicts conf))
	    (tn (global-conflicts-tn conf))
	    (num (global-conflicts-number conf))
	    (kind (global-conflicts-kind conf)))
	(setf (tn-local-number tn) num)
	(unless (eq kind :live)
	  (cond ((not (zerop (sbit live-bits num)))
		 (bit-vector-replace bits live-bits)
		 (setf (sbit bits num) 0)
		 (push-in tn-next* tn live-list))
		((and (eq (svref ltns num) :more)
		      (eq kind :write))
		 (note-conflicts live-bits live-list tn num)
		 (setf (sbit live-bits num) 1)
		 (push-in tn-next* tn live-list)
		 (setf (sbit live-in num) 1)))

	  (setf (tn-local-conflicts tn) bits))))

    (make-debug-environment-tns-live block live-bits live-list)))

;;; DO-SAVE-P-STUFF  --  Internal
;;;
;;; A function called in Conflict-Analyze-1-Block when we have a VOP with
;;; SAVE-P true.  We compute the save-set, and if :FORCE-TO-STACK, force
;;; all the live TNs to be stack environment TNs.
;;;
(defun do-save-p-stuff (vop block live-bits)
  (declare (type vop vop) (type ir2-block block)
	   (type local-tn-bit-vector live-bits))
  (let ((ss (compute-save-set vop live-bits)))
    (setf (vop-save-set vop) ss)
    (when (eq (vop-info-save-p (vop-info vop)) :force-to-stack)
      (do-live-tns (tn ss block)
	(unless (eq (tn-kind tn) :component)
	  (force-tn-to-stack tn)
	  (unless (eq (tn-kind tn) :environment)
	    (convert-to-environment-tn
	     tn
	     (block-environment (ir2-block-block block))))))))
  (undefined-value))

(eval-when (compile eval)

;;; Frob-More-TNs  --  Internal
;;;
;;; Used in SCAN-VOP-REFS to simultaneously do something to all of the TNs
;;; referenced by a big more arg.  We have to treat these TNs specially,
;;; since when we set or clear the bit in the live TNs, the represents a
;;; change in the liveness of all the more TNs.  If we iterated as normal,
;;; the next more ref would be thought to be not live when it was, etc.  We
;;; update Ref to be the last :more ref we scanned, so that the main loop
;;; will step to the next non-more ref.
;;;
(defmacro frob-more-tns (action)
  `(when (eq (svref ltns num) :more)
     (let ((prev ref))
       (do ((mref (tn-ref-next-ref ref) (tn-ref-next-ref mref)))
	   ((null mref))
	 (let ((mtn (tn-ref-tn mref)))
	   (unless (eql (tn-local-number mtn) num)
	     (return))
	   ,action)
	 (setq prev mref))
       (setq ref prev))))

;;; SCAN-VOP-REFS  --  Internal
;;;
;;; Handle the part of CONFLICT-ANALYZE-1-BLOCK that scans the REFs for the
;;; current VOP.  This macro references free variables in
;;; CONFLICT-ANALYZE-1-BLOCK.
;;;
(defmacro scan-vop-refs ()
  '(do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
       ((null ref))
     (let* ((tn (tn-ref-tn ref))
	    (num (tn-local-number tn)))
       (cond
	((not num))
	((not (zerop (sbit live-bits num)))
	 (when (tn-ref-write-p ref)
	   (setf (sbit live-bits num) 0)
	   (deletef-in tn-next* live-list tn)
	   (frob-more-tns (deletef-in tn-next* live-list mtn))))
	(t
	 (assert (not (tn-ref-write-p ref)))
	 (note-conflicts live-bits live-list tn num)
	 (frob-more-tns (note-conflicts live-bits live-list mtn num))
	 (setf (sbit live-bits num) 1)
	 (push-in tn-next* tn live-list)
	 (frob-more-tns (push-in tn-next* mtn live-list)))))))

;;; ENSURE-RESULTS-LIVE  --  Internal
;;;
;;; This macro is called by CONFLICT-ANALYZE-1-BLOCK to scan the current
;;; VOP's results, and make any dead ones live.  This is necessary, since
;;; even though a result is dead after the VOP, it may be in use for an
;;; extended period within the VOP (especially if it has :FROM specified.)
;;; During this interval, temporaries must be noted to conflict with the
;;; result.  More results are finessed in COMPUTE-INITIAL-CONFLICTS, so we
;;; ignore them here.
;;;
(defmacro ensure-results-live ()
  '(do ((res (vop-results vop) (tn-ref-across res)))
       ((null res))
     (let* ((tn (tn-ref-tn res))
	    (num (tn-local-number tn)))
       (when (and num (zerop (sbit live-bits num)))
	 (unless (eq (svref ltns num) :more)
	   (note-conflicts live-bits live-list tn num)
	   (setf (sbit live-bits num) 1)
	   (push-in tn-next* tn live-list))))))

); Eval-When (Compile Eval)

;;; Conflict-Analyze-1-Block  --  Internal
;;;
;;; Compute the block-local conflict information for Block.  We iterate
;;; over all the TN-Refs in a block in reference order, maintaining the set
;;; of live TNs in both a list and a bit-vector representation.
;;;
(defun conflict-analyze-1-block (block)
  (declare (type ir2-block block))
  (multiple-value-bind
      (live-bits live-list)
      (compute-initial-conflicts block)
    (let ((ltns (ir2-block-local-tns block)))
      (do ((vop (ir2-block-last-vop block)
		(vop-prev vop)))
	  ((null vop))
	(when (vop-info-save-p (vop-info vop))
	  (do-save-p-stuff vop block live-bits))
	(ensure-results-live)
	(scan-vop-refs)))))

;;; Lifetime-Post-Pass  --  Internal
;;;
;;; Conflict analyze each block, and also add it.
;;;
(defun lifetime-post-pass (component)
  (declare (type component component))
  (do-ir2-blocks (block component)
    (conflict-analyze-1-block block)))


;;;; Alias TN stuff.

;;; MERGE-ALIAS-BLOCK-CONFLICTS  --  Internal
;;;
;;; Destructively modify Oconf to include the conflict information in Conf.
;;;
(defun merge-alias-block-conflicts (conf oconf)
  (declare (type global-conflicts conf oconf))
  (let* ((kind (global-conflicts-kind conf))
	 (num (global-conflicts-number conf))
	 (okind (global-conflicts-kind oconf))
	 (onum (global-conflicts-number oconf))
	 (block (global-conflicts-block oconf))
	 (ltns (ir2-block-local-tns block)))
    (cond
     ((eq okind :live))
     ((eq kind :live)
      (setf (global-conflicts-kind oconf) :live)
      (setf (svref ltns onum) nil)
      (setf (global-conflicts-number oconf) nil))
     (t
      (unless (eq kind okind)
	(setf (global-conflicts-kind oconf) :read))
      ;;
      ;; Make original conflict with all the local TNs the alias conflicted
      ;; with.
      (bit-ior (global-conflicts-conflicts oconf)
	       (global-conflicts-conflicts conf)
	       t)
      (flet ((frob (x)
	       (unless (zerop (sbit x num))
		 (setf (sbit x onum) 1))))
	;;
	;; Make all the local TNs that conflicted with the alias conflict
	;; with the original.
	(dotimes (i (ir2-block-local-tn-count block))
	  (let ((tn (svref ltns i)))
	    (when (and tn (not (eq tn :more))
		       (null (tn-global-conflicts tn)))
	      (frob (tn-local-conflicts tn)))))
	;;
	;; Same for global TNs...
	(do ((current (ir2-block-global-tns block)
		      (global-conflicts-next current)))
	    ((null current))
	  (unless (eq (global-conflicts-kind current) :live)
	    (frob (global-conflicts-conflicts current))))
	;;
	;; Make the original TN live everywhere that the alias was live.
	(frob (ir2-block-written block))
	(frob (ir2-block-live-in block))
	(frob (ir2-block-live-out block))
	(do ((vop (ir2-block-start-vop block)
		  (vop-next vop)))
	    ((null vop))
	  (let ((sset (vop-save-set vop)))
	    (when sset (frob sset)))))))
    ;;
    ;; Delete the alias's conflict info.
    (when num
      (setf (svref ltns num) nil))
    (deletef-in global-conflicts-next (ir2-block-global-tns block) conf))

  (undefined-value))

;;; CHANGE-GLOBAL-CONFLICTS-TN  --  Internal
;;;
;;; Co-opt Conf to be a conflict for TN.
;;;
(defun change-global-conflicts-tn (conf new)
  (declare (type global-conflicts conf) (type tn new))
  (setf (global-conflicts-tn conf) new)
  (let ((ltn-num (global-conflicts-number conf))
	(block (global-conflicts-block conf)))
    (deletef-in global-conflicts-next (ir2-block-global-tns block) conf)
    (setf (global-conflicts-next conf) nil)
    (insert-block-global-conflict conf block)
    (when ltn-num
      (setf (svref (ir2-block-local-tns block) ltn-num) new)))
  (undefined-value))

;;; ENSURE-GLOBAL-TN  --  Internal
;;;
;;; Do CONVERT-TO-GLOBAL on TN if it has no global conflicts.  Copy the
;;; local conflicts into the global bit vector.
;;;
(defun ensure-global-tn (tn)
  (declare (type tn tn))
  (cond ((tn-global-conflicts tn))
	((tn-local tn)
	 (convert-to-global tn)
	 (bit-ior (global-conflicts-conflicts (tn-global-conflicts tn))
		  (tn-local-conflicts tn)
		  t))
	(t
	 (assert (and (null (tn-reads tn)) (null (tn-writes tn))))))
  (undefined-value))

;;; MERGE-ALIAS-CONFLICTS  --  Internal
;;;
;;; For each :ALIAS TN, destructively merge the conflict info into the
;;; original TN and replace the uses of the alias.
;;;
;;; For any block that uses only the alias TN, just insert that conflict
;;; into the conflicts for the original TN, changing the LTN map to refer
;;; to the original TN.  This gives a result indistinguishable from the
;;; what there would have been if the original TN had always been
;;; referenced.  This leaves no sign that an alias TN was ever involved.
;;;
;;; If a block has references to both the alias and the original TN, then
;;; we call MERGE-ALIAS-BLOCK-CONFLICTS to combine the conflicts into the
;;; original conflict.
;;;
(defun merge-alias-conflicts (component)
  (declare (type component component))
  (do ((tn (ir2-component-alias-tns (component-info component))
	   (tn-next tn)))
      ((null tn))
    (let ((original (tn-save-tn tn)))
      (ensure-global-tn tn)
      (ensure-global-tn original)
      (let ((conf (tn-global-conflicts tn))
	    (oconf (tn-global-conflicts original))
	    (oprev nil))
	(loop
	  (unless oconf
	    (if oprev
		(setf (global-conflicts-tn-next oprev) conf)
		(setf (tn-global-conflicts original) conf))
	    (do ((current conf (global-conflicts-tn-next current)))
		((null current))
	      (change-global-conflicts-tn current original))
	    (return))
	  (let* ((block (global-conflicts-block conf))
		 (num (ir2-block-number block))
		 (onum (ir2-block-number (global-conflicts-block oconf))))

	    (cond ((< onum num)
		   (shiftf oprev oconf (global-conflicts-tn-next oconf)))
		  ((> onum num)
		   (if oprev
		       (setf (global-conflicts-tn-next oprev) conf)
		       (setf (tn-global-conflicts original) conf))
		   (change-global-conflicts-tn conf original)
		   (shiftf oprev conf (global-conflicts-tn-next conf) oconf))
		  (t
		   (merge-alias-block-conflicts conf oconf)
		   (shiftf oprev oconf (global-conflicts-tn-next oconf))
		   (setf conf (global-conflicts-tn-next conf)))))
	  (unless conf (return))))

      (flet ((frob (refs)
	       (let ((ref refs)
		     (next nil))
		 (loop
		   (unless ref (return))
		   (setq next (tn-ref-next ref))
		   (change-tn-ref-tn ref original)
		   (setq ref next)))))
	(frob (tn-reads tn))
	(frob (tn-writes tn)))
      (setf (tn-global-conflicts tn) nil)))

  (undefined-value))


;;; Lifetime-Analyze  --  Interface
;;;
;;;
(defun lifetime-analyze (component)
  (lifetime-pre-pass component)
  (setup-environment-live-conflicts component)
  (lifetime-flow-analysis component)
  (lifetime-post-pass component)
  (merge-alias-conflicts component))


;;;; Conflict testing.

;;; TNs-Conflict-Local-Global  --  Internal
;;;
;;; Test for a conflict between the local TN X and the global TN Y.  We
;;; just look for a global conflict of Y in X's block, and then test for
;;; conflict in that block.
;;; [### Might be more efficient to scan Y's global conflicts.  This
;;; depends on whether there are more global TNs than blocks.]
;;;
(defun tns-conflict-local-global (x y)
  (let ((block (tn-local x)))
    (do ((conf (ir2-block-global-tns block)
	       (global-conflicts-next conf)))
	((null conf) nil)
      (when (eq (global-conflicts-tn conf) y)
	(let ((num (global-conflicts-number conf)))
	  (return (or (not num)
		      (not (zerop (sbit (tn-local-conflicts x)
					num))))))))))

;;; TNs-Conflict-Global-Global  --  Internal
;;;
;;; Test for conflict between two global TNs X and Y.
;;;
(defun tns-conflict-global-global (x y)
  (declare (type tn x y))
  (let* ((x-conf (tn-global-conflicts x))
	 (x-num (ir2-block-number (global-conflicts-block x-conf)))
	 (y-conf (tn-global-conflicts y))
	 (y-num (ir2-block-number (global-conflicts-block y-conf))))

    (macrolet ((advance (n c)
		 `(progn
		    (setq ,c (global-conflicts-tn-next ,c))
		    (unless ,c (return-from tns-conflict-global-global nil))
		    (setq ,n (ir2-block-number (global-conflicts-block ,c)))))
	       (scan (g l lc)
		 `(do ()
		      ((>= ,g ,l))
		    (advance ,l ,lc))))

      (loop
	;; x-conf, y-conf true, x-num, y-num corresponding block numbers.
	(scan x-num y-num y-conf)
	(scan y-num x-num x-conf)
	(when (= x-num y-num)
	  (let ((ltn-num-x (global-conflicts-number x-conf)))
	    (unless (and ltn-num-x
			 (global-conflicts-number y-conf)
			 (zerop (sbit (global-conflicts-conflicts y-conf)
				      ltn-num-x)))
	      (return t))
	    (advance x-num x-conf)
	    (advance y-num y-conf)))))))

;;; TNs-Conflict  --  Interface
;;;
;;; Return true if X and Y are distinct and the lifetimes of X and Y
;;; overlap at any point.
;;;
(defun tns-conflict (x y)
  (declare (type tn x y))
  (let ((x-kind (tn-kind x))
	(y-kind (tn-kind y)))
    (cond ((eq x y) nil)
	  ((or (eq x-kind :component) (eq y-kind :component)) t)
	  ((tn-global-conflicts x)
	   (if (tn-global-conflicts y)
	       (tns-conflict-global-global x y)
	       (tns-conflict-local-global y x)))
	  ((tn-global-conflicts y)
	   (tns-conflict-local-global x y))
	  (t
	   (and (eq (tn-local x) (tn-local y))
		(not (zerop (sbit (tn-local-conflicts x)
				  (tn-local-number y)))))))))
