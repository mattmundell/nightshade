;;; Implementation independent code for Pack phase in the compiler.  Pack
;;; is responsible for assigning TNs to storage allocations or "register
;;; allocation".

(in-package "C")

(declaim (optimize (inhibit-warnings 1)))

#[ Packing

    Find a legal register allocation, attempting to minimize unnecessary
    moves.

Phase position: 19/23 (back)

Presence: required

Files: pack

Entry functions: `pack'

Call sequences:

    native-compile-component
      pack
        init-sb-vectors
        pack-wired-tn
          grow-sc
          add-location-conflicts
        pack-tn
          select-location
          grow-sc
          add-location-conflicts
        assign-tn-costs
        optimized-emit-saves
        pack-load-tns
          check-operand-restrictions
        emit-saves

    Add lifetime/pack support for pre-packed save TNs.

    Fix GTN/VMR conversion to use pre-packed save TNs for old-cont and return-PC.
    (Will prevent preference from passing location to save location from ever being
    honored?)

    We will need to make packing of passing locations smarter before we will be
    able to target the passing location on the stack in a tail call (when that is
    where the callee wants it.)  Currently, we will almost always pack the passing
    location in a register without considering whether that is really a good idea.
    Maybe we should consider schemes that explicitly understand the parallel
    assignment semantics, and try to do the assignment with a minimum number of
    temporaries.  We only need assignment temps for TNs that appear both as an
    actual argument value and as a formal parameter of the called function.  This
    only happens in self-recursive functions.

    Could be a problem with lifetime analysis, though.  The write by a move-arg VOP
    would look like a write in the current env, when it really isn't.  If this is a
    problem, then we might want to make the result TN be an info arg rather than a
    real operand.  But this would only be a problem in recursive calls, anyway.
	This would prevent targeting, but targeting across passing locations rarely
	seems to work anyway.
	    XXX But the :ENVIRONMENT TN mechanism would get confused.  Maybe put
	    env explicitly in TN, and have it only always-live in that
	    env, and normal in other envs (or blocks it is written in.)  This would
	    allow targeting into environment TNs.

    I guess we would also want the env/PC save TNs normal in the return block so
    that we can target them.  We could do this by considering env TNs normal in
    read blocks with no successors.

    ENV TNs would be treated totally normally in non-env blocks, so we don't have
    to worry about lifetime analysis getting confused by variable initializations.
    Do some kind of TN costing to determine when it is more trouble than it is
    worth to allocate TNs in registers.

    Change pack ordering to be less pessimal.  Pack TNs as they are seen in the LTN
    map in DFO, which at least in non-block compilations has an effect something
    like packing main trace TNs first, since control analysis tries to put the good
    code first.  This could also reduce spilling, since it makes it less likely we
    will clog all registers with global TNs.

    If we pack a TN with a specified save location on the stack, pack in the
    specified location.

    Allow old-cont and return-pc to be kept in registers by adding a new "keep
    around" kind of TN.  These are kind of like environment live, but are only
    always-live in blocks that they weren't referenced in.  Lifetime analysis does
    a post-pass adding always-live conflicts for each "keep around" TN to those
    blocks with no conflict for that TN.  The distinction between always-live and
    keep-around allows us to successfully target old-cont and return-pc to passing
    locations.  MAKE-KEEP-AROUND-TN (ptype), PRE-PACK-SAVE-TN (tn scn offset).
    Environment needs a KEEP-AROUND-TNS slot so that conflict analysis can find
    them (no special casing is needed after then, they can be made with :NORMAL
    kind).  VMR-component needs PRE-PACKED-SAVE-TNS so that conflict analysis or
    somebody can copy conflict info from the saved TN.



    Note that having block granularity in the conflict information doesn't mean
    that a localized packing scheme would have to do all moves at block boundaries
    (which would clash with the desire the have saving done as part of this
    mechanism.)  All that it means is that if we want to do a move within the
    block, we would need to allocate both locations throughout that block (or
    something).



    Load TN pack:

    A location is out for load TN packing if:

    The location has TN live in it after the VOP for a result, or before the VOP
    for an argument, or

    The location is used earlier in the TN-ref list (after) the saved results ref
    or later in the TN-Ref list (before) the loaded argument's ref.

    To pack load TNs, we advance the live-tns to the interesting VOP, then
    repeatedly scan the vop-refs to find vop-local conflicts for each needed load
    TN.  We insert move VOPs and change over the TN-Ref-TNs as we go so the TN-Refs
    will reflect conflicts with already packed load-TNs.

    If we fail to pack a load-TN in the desired SC, then we scan the Live-TNs for
    the SB, looking for a TN that can be packed in an unbounded SB.  This TN must
    then be repacked in the unbounded SB.  It is important the load-TNs are never
    packed in unbounded SBs, since that would invalidate the conflicts info,
    preventing us from repacking TNs in unbounded SBs.  We can't repack in a finite
    SB, since there might have been load TNs packed in that SB which aren't
    represented in the original conflict structures.

    Is it permissible to "restrict" an operand to an unbounded SC?  Not impossible
    to satisfy as long as a finite SC is also allowed.  But in practice, no
    restriction would probably be as good.

    We assume all locations can be used when an sc is based on an unbounded sb.


    TN-Refs are be convenient structures to build the target graph out of.  If we
    allocated space in every TN-Ref, then there would certainly be enough to
    represent arbitrary target graphs.  Would it be enough to allocate a single
    Target slot?  If there is a target path though a given VOP, then the Target of
    the write ref would be the read, and vice-versa.  To find all the TNs that
    target us, we look at the TN for the target of all our write refs.

    We separately chain together the read refs and the write refs for a TN,
    allowing easy determination of things such as whether a TN has only a single
    definition or has no reads.  It would also allow easier traversal of the target
    graph.

    Represent per-location conflicts as vectors indexed by block number of
    per-block conflict info.  To test whether a TN conflicts on a location, we
    would then have to iterate over the TNs global-conflicts, using the block
    number and LTN number to check for a conflict in that block.  But since most
    TNs are local, this test actually isn't much more expensive than indexing into
    a bit-vector by GTN numbers.

    The big win of this scheme is that it is much cheaper to add conflicts into the
    conflict set for a location, since we never need to actually compute the
    conflict set in a list-like representation (which requires iterating over the
    LTN conflicts vectors and unioning in the always-live TNs).  Instead, we just
    iterate over the global-conflicts for the TN, using BIT-IOR to combine the
    conflict set with the bit-vector for that block in that location, or marking
    that block/location combination as being always-live if the conflict is
    always-live.

    Generating the conflict set is inherently more costly, since although we
    believe the conflict set size to be roughly constant, it can easily contain
    tens of elements.  We would have to generate these moderately large lists for
    all TNs, including local TNs.  In contrast, the proposed scheme does work
    proportional to the number of blocks the TN is live in, which is small on
    average (1 for local TNs).  This win exists independently from the win of not
    having to iterate over LTN conflict vectors.


	XXX Note that since we never do bitwise iteration over the LTN conflict
	vectors, part of the motivation for keeping these a small fixed size has been
	removed.  But it would still be useful to keep the size fixed so that we can
	easily recycle the bit-vectors, and so that we could potentially have maximally
	tense special primitives for doing clear and bit-ior on these vectors.

    This scheme is somewhat more space-intensive than having a per-location
    bit-vector.  Each vector entry would be something like 150 bits rather than one
    bit, but this is mitigated by the number of blocks being 5-10x smaller than the
    number of TNs.  This seems like an acceptable overhead, a small fraction of the
    total VMR representation.

    The space overhead could also be reduced by using something equivalent to a
    two-dimensional bit array, indexed first by LTN numbers, and then block numbers
    (instead of using a simple-vector of separate bit-vectors.)  This would
    eliminate space wastage due to bit-vector overheads, which might be 50% or
    more, and would also make efficient zeroing of the vectors more
    straightforward.  We would then want efficient operations for OR'ing LTN
    conflict vectors with rows in the array.

    This representation also opens a whole new range of allocation algorithms: ones
    that store allocate TNs in different locations within different portions of the
    program.  This is because we can now represent a location being used to hold a
    certain TN within an arbitrary subset of the blocks the TN is referenced in.



Pack goals:

Pack should:

Subject to resource constraints:
 * Minimize use costs
     -- "Register allocation"
         Allocate as many values as possible in scarce "good" locations,
         attempting to minimize the aggregate use cost for the entire program.
     -- "Save optimization"
         Don't allocate values in registers when the save/restore costs exceed
         the expected gain for keeping the value in a register.  (Similar to
         "opening costs" in RAOC (FIX is this rabbit?).)
             Really just a case of representation selection.

 * Minimize preference costs
   Eliminate as many moves as possible.

"Register allocation" is basically an attempt to eliminate moves between
registers and memory.  "Save optimization" counterbalances "register
allocation" to prevent it from becoming a pessimization, since saves can
introduce register/memory moves.

Preference optimization reduces the number of moves within an SC.  Doing a good
job of honoring preferences is important to the success of the compiler, since
we have assumed in many places that moves will usually be optimized away.

The scarcity-oriented aspect of "register allocation" is handled by a greedy
algorithm in pack.  We try to pack the "most important" TNs first, under the
theory that earlier packing is more likely to succeed due to fewer constraints.

The drawback of greedy algorithms is their inability to look ahead.  Packing a
TN may mess up later "register allocation" by precluding packing of TNs that
are individually "less important", but more important in aggregate.  Packing a
TN may also prevent preferences from being honored.


== Initial packing ==

Pack all TNs restricted to a finite SC first, before packing any other TNs.

One might suppose that Pack would have to treat TNs in different environments
differently, but this is not the case.  Pack simply assigns TNs to locations so
that no two conflicting TNs are in the same location.  In the process of
implementing call semantics in conflict analysis (FIX lifetime analysis?), we cause TNs in different
environments not to conflict.  In the case of passing TNs, cross environment
conflicts do exist, but this reflects reality, since the passing TNs are
live in both the caller and the callee.  Environment semantics has already been
implemented at this point.

This means that Pack can pack all TNs simultaneously, using one data
structure to represent the conflicts for each location.  So we have only
one conflict set per SB location, rather than separating this information
by environment.


== Load TN packing ==

We create load TNs as needed in a post-pass to the initial packing.  After TNs
are packed, it may be that some references to a TN will require it to be in a
SC other than the one it was packed in.  We create load-TNs and pack them on
the fly during this post-pass.

What we do is have an optional SC restriction associated with TN-refs.  If we
pack the TN in an SC which is different from the required SC for the reference,
then we create a TN for each such reference, and pack it into the required SC.

In many cases we will be able to pack the load TN with no hassle, but in
general we may need to spill a TN that has already been packed.  We choose a
TN that isn't in use by the offending VOP, and then spill that TN onto the
stack for the duration of that VOP.  If the VOP is a conditional, then we must
insert a new block interposed before the branch target so that the value TN
value is restored regardless of which branch is taken.

Instead of remembering lifetime information from conflict analysis, we rederive
it.  We scan each block backward while keeping track of which locations have
live TNs in them.  When we find a reference that needs a load TN packed, we try
to pack it in an unused location.  If we can't, we unpack the currently live TN
with the lowest cost and force it into an unbounded SC.

The per-location and per-TN conflict information used by pack doesn't
need to be updated when we pack a load TN, since we are done using those data
structures.

We also don't need to create any TN-Refs for load TNs.
    ??? How do we keep
    track of load-tn lifetimes?  It isn't really that hard, I guess.  We just
    remember which load TNs we created at each VOP, killing them when we pass the
    loading (or saving) step.  This suggests we could flush the Refs thread if we
    were willing to sacrifice some flexibility in explicit temporary lifetimes.
    Flushing the Refs would make creating the VMR representation easier.

The lifetime analysis done during load-TN packing doubles as a consistency
check.  If we see a read of a TN packed in a location which has a different TN
currently live, then there is a packing bug.  If any of the TNs recorded as
being live at the block beginning are packed in a scarce SB, but aren't current
in that location, then we also have a problem.

The conflict structure for load TNs is fairly simple, the load TNs for
arguments and results all conflict with each other, and don't conflict with
much else.  We just try packing in targeted locations before trying at random.
]#

;;; Some parameters controlling which optimizations we attempt (for debugging.)
;;;
(defparameter pack-assign-costs t)
(defparameter pack-optimize-saves t)
(defparameter pack-save-once t)

(declaim (ftype (function (component) index) ir2-block-count))


;;;; Conflict determination.

(declaim (start-block offset-conflicts-in-sb conflicts-in-sc))

;;; Offset-Conflicts-In-SB  --  Internal
;;;
;;; Return true if the element at the specified offset in SB has a conflict
;;; with TN:
;;; -- If an component-live TN (:component kind), then iterate over all the
;;;    blocks.  If the element at Offset is used anywhere in any of the
;;;    component's blocks (always-live /= 0), then there is a conflict.
;;; -- If TN is global (Confs true), then iterate over the blocks TN is live in
;;;    (using TN-Global-Conflicts).  If the TN is live everywhere in the block
;;;    (:Live), then there is a conflict if the element at offset is used
;;;    anywhere in the block (Always-Live /= 0).  Otherwise, we use the local
;;;    TN number for TN in block to find whether TN has a conflict at Offset in
;;;    that block.
;;; -- If TN is local, then we just check for a conflict in the block it is
;;;    local to.
;;;
(defun offset-conflicts-in-sb (tn sb offset)
  (declare (type tn tn) (type finite-sb sb) (type index offset))
  (let ((confs (tn-global-conflicts tn))
	(kind (tn-kind tn)))
    (cond
     ((eq kind :component)
      (let ((loc-live (svref (finite-sb-always-live sb) offset)))
	(dotimes (i (ir2-block-count *compile-component*) nil)
	  (when (/= (sbit loc-live i) 0)
	    (return t)))))
     (confs
      (let ((loc-confs (svref (finite-sb-conflicts sb) offset))
	    (loc-live (svref (finite-sb-always-live sb) offset)))
	(do ((conf confs (global-conflicts-tn-next conf)))
	    ((null conf)
	     nil)
	  (let* ((block (global-conflicts-block conf))
		 (num (ir2-block-number block)))
	    (if (eq (global-conflicts-kind conf) :live)
		(when (/= (sbit loc-live num) 0)
		  (return t))
		(when (/= (sbit (svref loc-confs num)
				(global-conflicts-number conf))
			  0)
		  (return t)))))))
     (t
      (/= (sbit (svref (svref (finite-sb-conflicts sb) offset)
		       (ir2-block-number (tn-local tn)))
		(tn-local-number tn))
	  0)))))

;;; Conflicts-In-SC  --  Internal
;;;
;;; Return true if TN has a conflict in SC at the specified offset.
;;;
(defun conflicts-in-sc (tn sc offset)
  (declare (type tn tn) (type sc sc) (type index offset))
  (let ((sb (sc-sb sc)))
    (dotimes (i (sc-element-size sc) nil)
      (when (offset-conflicts-in-sb tn sb (+ offset i))
	(return t)))))

(declaim (end-block))

;;; Add-Location-Conflicts  --  Internal
;;;
;;; Add TN's conflicts into the conflicts for the location at Offset in SC.
;;; We iterate over each location in TN, adding to the conflicts for that
;;; location:
;;; -- If TN is a :Component TN, then iterate over all the blocks, setting
;;;    all of the local conflict bits and the always-live bit.  This records a
;;;    conflict with any TN that has a LTN number in the block, as well as with
;;;    :Always-Live and :Environment TNs.
;;; -- If TN is global, then iterate over the blocks TN is live in.  In
;;;    addition to setting the always-live bit to represent the conflict with
;;;    TNs live throughout the block, we also set bits in the local conflicts.
;;;    If TN is :Always-Live in the block, we set all the bits, otherwise we or
;;;    in the local conflict bits.
;;; -- If the TN is local, then we just do the block it is local to, setting
;;;    always-live and OR'ing in the local conflicts.
;;;
(defun add-location-conflicts (tn sc offset)
  (declare (type tn tn) (type sc sc) (type index offset))
  (let ((confs (tn-global-conflicts tn))
	(sb (sc-sb sc))
	(kind (tn-kind tn)))
    (dotimes (i (sc-element-size sc))
      (declare (type index i))
      (let* ((this-offset (+ offset i))
	     (loc-confs (svref (finite-sb-conflicts sb) this-offset))
	     (loc-live (svref (finite-sb-always-live sb) this-offset)))
	(cond
	 ((eq kind :component)
	  (dotimes (num (ir2-block-count *compile-component*) nil)
	    (declare (type index num))
	    (setf (sbit loc-live num) 1)
	    (set-bit-vector (svref loc-confs num))))
	 (confs
	  (do ((conf confs (global-conflicts-tn-next conf)))
	      ((null conf))
	    (let* ((block (global-conflicts-block conf))
		   (num (ir2-block-number block))
		   (local-confs (svref loc-confs num)))
	      (declare (type local-tn-bit-vector local-confs))
	      (setf (sbit loc-live num) 1)
	      (if (eq (global-conflicts-kind conf) :live)
		  (set-bit-vector local-confs)
		  (bit-ior local-confs (global-conflicts-conflicts conf) t)))))
	 (t
	  (let ((num (ir2-block-number (tn-local tn))))
	    (setf (sbit loc-live num) 1)
	    (bit-ior (the local-tn-bit-vector (svref loc-confs num))
		     (tn-local-conflicts tn) t))))))))

;;; IR2-BLOCK-COUNT  --  Internal
;;;
;;; Return the total number of IR2 blocks in Component.
;;;
(defun ir2-block-count (component)
  (declare (type component component))
  (do ((2block (block-info (block-next (component-head component)))
	       (ir2-block-next 2block)))
      ((null 2block)
       (error "What?  No ir2 blocks have a true number?"))
    (if (ir2-block-number 2block)
	(return (1+ (ir2-block-number 2block))))))

;;; Init-SB-Vectors  --  Internal
;;;
;;; Ensure that the conflicts vectors for each :Finite SB are large enough
;;; for the number of blocks allocated.  Also clear any old conflicts and
;;; reset the current size to the initial size.
;;;
(defun init-sb-vectors (component)
  (let ((nblocks (ir2-block-count component)))
    (dolist (sb (backend-sb-list *backend*))
      (unless (eq (sb-kind sb) :non-packed)
	(let* ((conflicts (finite-sb-conflicts sb))
	       (always-live (finite-sb-always-live sb))
	       (max-locs (length conflicts))
	       (last-count (finite-sb-last-block-count sb)))
	  (unless (zerop max-locs)
	    (let ((current-size (length (the simple-vector
					     (svref conflicts 0)))))
	      (cond
	       ((> nblocks current-size)
		(let ((new-size (max nblocks (* current-size 2))))
		  (declare (type index new-size))
		  (dotimes (i max-locs)
		    (declare (type index i))
		    (let ((new-vec (make-array new-size)))
		      (let ((old (svref conflicts i)))
			(declare (simple-vector old))
			(dotimes (j current-size)
			  (declare (type index j))
			  (setf (svref new-vec j)
				(clear-ltn-bit-vector (svref old j)))))

		      (do ((j current-size (1+ j)))
			  ((= j new-size))
			(declare (type index j))
			(setf (svref new-vec j)
			      (make-array local-tn-limit :element-type 'bit
					  :initial-element 0)))
		      (setf (svref conflicts i) new-vec))
		    (setf (svref always-live i)
			  (make-array new-size :element-type 'bit
				      :initial-element 0)))))
	       (t
		(dotimes (i (finite-sb-current-size sb))
		  (declare (type index i))
		  (let ((conf (svref conflicts i)))
		    (declare (simple-vector conf))
		    (dotimes (j last-count)
		      (declare (type index j))
		      (clear-ltn-bit-vector (svref conf j))))
		  (clear-bit-vector (svref always-live i)))))))

	  (setf (finite-sb-last-block-count sb) nblocks)
	  (setf (finite-sb-current-size sb) (sb-size sb))
	  (setf (finite-sb-last-offset sb) 0))))))

;;; Grow-SC  --  Internal
;;;
;;; Expand the :Unbounded SB backing SC by either the initial size or the
;;; SC element size, whichever is larger.  If Needed-Size is larger, then
;;; use that size.
;;;
(defun grow-sc (sc &optional (needed-size 0))
  (declare (type sc sc) (type index needed-size))
  (let* ((sb (sc-sb sc))
	 (size (finite-sb-current-size sb))
	 (align-mask (1- (sc-alignment sc)))
	 (inc (max (sb-size sb)
		   (+ (sc-element-size sc)
		      (- (logandc2 (+ size align-mask) align-mask)
			 size))
		   (- needed-size size)))
	 (new-size (+ size inc))
	 (conflicts (finite-sb-conflicts sb))
	 (block-size (if (zerop (length conflicts))
			 (ir2-block-count *compile-component*)
			 (length (the simple-vector (svref conflicts 0))))))
    (declare (type index inc new-size))
    (assert (eq (sb-kind sb) :unbounded))

    (when (> new-size (length conflicts))
      (let ((new-conf (make-array new-size)))
	(replace new-conf conflicts)
	(do ((i size (1+ i)))
	    ((= i new-size))
	  (declare (type index i))
	  (let ((loc-confs (make-array block-size)))
	    (dotimes (j block-size)
	      (setf (svref loc-confs j)
		    (make-array local-tn-limit
				:initial-element 0
				:element-type 'bit)))
	    (setf (svref new-conf i) loc-confs)))
	(setf (finite-sb-conflicts sb) new-conf))

      (let ((new-live (make-array new-size)))
	(replace new-live (finite-sb-always-live sb))
	(do ((i size (1+ i)))
	    ((= i new-size))
	  (setf (svref new-live i)
		(make-array block-size
			    :initial-element 0
			    :element-type 'bit)))
	(setf (finite-sb-always-live sb) new-live))

      (let ((new-tns (make-array new-size :initial-element nil)))
	(replace new-tns (finite-sb-live-tns sb))
	(fill (finite-sb-live-tns sb) nil)
	(setf (finite-sb-live-tns sb) new-tns)))

    (setf (finite-sb-current-size sb) new-size))
  (undefined-value))

;;; This variable is true whenever we are in pack (and thus the per-SB
;;; conflicts information is in use.)
;;;
(defvar *in-pack* nil)

;;; Pack-Before-GC-Hook  --  Internal
;;;
;;; In order to prevent the conflict data structures from growing
;;; arbitrarily large, we clear them whenever a GC happens and we aren't
;;; currently in pack.  We revert to the initial number of locations and 0
;;; blocks.
;;;
(defun pack-before-gc-hook ()
  (unless *in-pack*
    (dolist (sb (backend-sb-list *backend*))
      (unless (eq (sb-kind sb) :non-packed)
	(let ((size (sb-size sb)))
	  (fill nil (finite-sb-always-live sb))
	  (setf (finite-sb-always-live sb)
		(make-array size :initial-element #*))

	  (fill nil (finite-sb-conflicts sb))
	  (setf (finite-sb-conflicts sb)
		(make-array size :initial-element '#()))

	  (fill nil (finite-sb-live-tns sb))
	  (setf (finite-sb-live-tns sb)
		(make-array size :initial-element nil))))))
  (undefined-value))

(pushnew 'pack-before-gc-hook ext:*before-gc-hooks*)


;;;; Internal errors.

(declaim (optimize (inhibit-warnings 2)))

;;; NO-LOAD-FUNCTION-ERROR  --  Internal
;;;
;;; Give someone a hard time because there isn't any load function defined
;;; to move from Src to Dest.
;;;
(defun no-load-function-error (src dest)
  (let* ((src-sc (tn-sc src))
	 (src-name (sc-name src-sc))
	 (dest-sc (tn-sc dest))
	 (dest-name (sc-name dest-sc)))
    (cond ((eq (sb-kind (sc-sb src-sc)) :non-packed)
	   (unless (member src-sc (sc-constant-scs dest-sc))
	     (error "Loading from an invalid constant SC?~@
	             VM definition inconsistent, try recompiling."))
	   (error "No load function defined to load SC ~S ~
	           from its constant SC ~S."
		  dest-name src-name))
	  ((member src-sc (sc-alternate-scs dest-sc))
	   (error "No load function defined to load SC ~S from its ~
	           alternate SC ~S."
		  dest-name src-name))
	  ((member dest-sc (sc-alternate-scs src-sc))
	   (error "No load function defined to save SC ~S in its ~
	           alternate SC ~S."
		  src-name dest-name))
	  (t
	   (error "Loading to/from SCs that aren't alternates?~@
	           VM definition inconsistent, try recompiling.")))))

;;; FAILED-TO-PACK-ERROR  --  Internal
;;;
;;; Called when we failed to pack TN.  If Restricted is true, then we we
;;; restricted to pack TN in its SC.
;;;
(defun failed-to-pack-error (tn restricted)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
	 (scs (cons sc (sc-alternate-scs sc))))
    (cond
     (restricted
      (error "Failed to pack restricted TN ~S in its SC ~S."
	     tn (sc-name sc)))
     (t
      (assert (not (find :unbounded scs
			 :key #'(lambda (x) (sb-kind (sc-sb x))))))
      (let ((ptype (tn-primitive-type tn)))
	(cond
	 (ptype
	  (assert (member (sc-number sc) (primitive-type-scs ptype)))
	  (error "SC ~S doesn't have any :Unbounded alternate SCs, but is~@
	          a SC for primitive-type ~S."
		 (sc-name sc) (primitive-type-name ptype)))
	 (t
	  (error "SC ~S doesn't have any :Unbounded alternate SCs."
		 (sc-name sc)))))))))

;;; DESCRIBE-TN-USE  --  Internal
;;;
;;; Return a list of format arguments describing how TN is used in Op's
;;; VOP.
;;;
(defun describe-tn-use (loc tn op)
  (let* ((vop (tn-ref-vop op))
	 (args (vop-args vop))
	 (results (vop-results vop))
	 (name (with-output-to-string (stream)
		 (print-tn tn stream)))
	 (2comp (component-info *compile-component*))
	 temp)
    (cond
     ((setq temp (position-in #'tn-ref-across tn args :key #'tn-ref-tn))
      `("~2D: ~A (~:R argument)" ,loc ,name ,(1+ temp)))
     ((setq temp (position-in #'tn-ref-across tn results :key #'tn-ref-tn))
      `("~2D: ~A (~:R result)" ,loc ,name ,(1+ temp)))
     ((setq temp (position-in #'tn-ref-across tn args :key #'tn-ref-load-tn))
      `("~2D: ~A (~:R argument load TN)" ,loc ,name ,(1+ temp)))
     ((setq temp (position-in #'tn-ref-across tn results :key
			      #'tn-ref-load-tn))
      `("~2D: ~A (~:R result load TN)" ,loc ,name ,(1+ temp)))
     ((setq temp (position-in #'tn-ref-across tn (vop-temps vop)
			      :key #'tn-ref-tn))
      `("~2D: ~A (temporary ~A)" ,loc ,name
	,(operand-parse-name (elt (vop-parse-temps
				   (vop-parse-or-lose
				    (vop-info-name  (vop-info vop))))
				  temp))))
     ((eq (tn-kind tn) :component)
      `("~2D: ~A (component live)" ,loc ,name))
     ((position-in #'tn-next tn (ir2-component-wired-tns 2comp))
      `("~2D: ~A (wired)" ,loc ,name))
     ((position-in #'tn-next tn (ir2-component-restricted-tns 2comp))
      `("~2D: ~A (restricted)" ,loc ,name))
     (t
      `("~2D: not referenced?" ,loc)))))

;;; FAILED-TO-PACK-LOAD-TN-ERROR  --  Internal
;;;
;;; If load TN packing fails, try to give a helpful error message.  We find
;;; a TN in each location that conflicts, and print it.
;;;
(defun failed-to-pack-load-tn-error (scs op)
  (declare (list scs) (type tn-ref op))
  (collect ((used)
	    (unused))
    (dolist (sc scs)
      (let* ((sb (sc-sb sc))
	     (confs (finite-sb-live-tns sb)))
	(assert (eq (sb-kind sb) :finite))
	(dolist (el (sc-locations sc))
	  (declare (type index el))
	  (let ((conf (load-tn-conflicts-in-sc op sc el t)))
	    (if conf
		(used (describe-tn-use el conf op))
		(do ((i el (1+ i))
		     (end (+ el (sc-element-size sc))))
		    ((= i end)
		     (unused el))
		  (declare (type index i end))
		  (let ((victim (svref confs i)))
		    (when victim
		      (used (describe-tn-use el victim op))
		      (return t)))))))))

    (multiple-value-bind (arg-p n more-p costs load-scs incon)
			 (get-operand-info op)
	(declare (ignore costs load-scs))
	(assert (not more-p))
	(error "Unable to pack a Load-TN in SC ~{~A~#[~^~;, or ~:;,~]~} ~
		for the ~:R ~:[result~;argument~] to~@
		the ~S VOP,~@
		~:[since all SC elements are in use:~:{~%~@?~}~%~;~
		~:*but these SC elements are not in use:~%  ~S~%Bug?~*~]~
		~:[~;~@
		Current cost info inconsistent with that in effect at compile ~
		time.  Recompile.~%Compilation order may be incorrect.~]"
	       (mapcar #'sc-name scs)
	       n arg-p
	       (vop-info-name (vop-info (tn-ref-vop op)))
	       (unused) (used)
	       incon))))

;;; NO-LOAD-SCS-ALLOWED-BY-PRIMITIVE-TYPE-ERROR  --  Internal
;;;
;;; Called when none of the SCs that we can load Op into are allowed by
;;; Op's primitive-type.
;;;
(defun no-load-scs-allowed-by-primitive-type-error (ref)
  (declare (type tn-ref ref))
  (let* ((tn (tn-ref-tn ref))
	 (ptype (tn-primitive-type tn)))
    (multiple-value-bind (arg-p pos more-p costs load-scs incon)
			 (get-operand-info ref)
      (declare (ignore costs))
      (assert (not more-p))
      (error "~S is not valid as the ~:R ~:[result~;argument~] to VOP:~
              ~%  ~S,~@
	      since the TN's primitive type ~S doesn't allow any of the SCs~@
	      allowed by the operand restriction:~%  ~S~
	      ~:[~;~@
	      Current cost info inconsistent with that in effect at compile ~
	      time.  Recompile.~%Compilation order may be incorrect.~]"
	     tn pos arg-p
	     (template-name (vop-info (tn-ref-vop ref)))
	     (primitive-type-name ptype)
	     (mapcar #'sc-name (listify-restrictions load-scs))
	     incon))))

(declaim (optimize (inhibit-warnings 1)))


;;;; Register saving.

(declaim (start-block optimized-emit-saves emit-saves assign-tn-costs
		      pack-save-tn))

;;; Note-Spilled-TN  --  Internal
;;;
;;; Do stuff to note that TN is spilled at VOP for the debugger's benefit.
;;;
(defun note-spilled-tn (tn vop)
  (when (and (tn-leaf tn) (vop-save-set vop))
    (let ((2comp (component-info *compile-component*)))
      (setf (gethash tn (ir2-component-spilled-tns 2comp)) t)
      (pushnew tn (gethash vop (ir2-component-spilled-vops 2comp)))))
  (undefined-value))

;;; Pack-Save-TN  --  Internal
;;;
;;; Make a save TN for TN, pack it, and return it.  We copy various
;;; conflict information from the TN so that pack does the right thing.
;;;
(defun pack-save-tn (tn)
  (declare (type tn tn))
  (let ((res (make-tn 0 :save nil nil)))
    (dolist (alt (sc-alternate-scs (tn-sc tn))
		 (error "No unbounded alternate for SC ~S."
			(sc-name (tn-sc tn))))
      (when (eq (sb-kind (sc-sb alt)) :unbounded)
	(setf (tn-save-tn tn) res)
	(setf (tn-save-tn res) tn)
	(setf (tn-sc res) alt)
	(pack-tn res t)
	(return res)))))

;;; EMIT-OPERAND-LOAD  --  Internal
;;;
;;; Find the load function for moving from Src to Dest and emit a
;;; MOVE-OPERAND VOP with that function as its info arg.
;;;
(defun emit-operand-load (node block src dest before)
  (declare (type node node) (type ir2-block block)
	   (type tn src dest) (type (or vop null) before))
  (emit-load-template node block
		      (template-or-lose 'move-operand *backend*)
		      src dest
		      (list (or (svref (sc-move-functions (tn-sc dest))
				       (sc-number (tn-sc src)))
				(no-load-function-error src dest)))
		      before)
  (undefined-value))

;;; REVERSE-FIND-VOP  --  Internal
;;;
;;; Find the preceding use of the VOP NAME in the emit order, starting with
;;; VOP.  We must find the VOP in the same IR1 block.
;;;
(defun reverse-find-vop (name vop)
  (do* ((block (vop-block vop) (ir2-block-prev block))
	(last vop (ir2-block-last-vop block)))
       (nil)
    (assert (eq (ir2-block-block block) (ir2-block-block (vop-block vop))))
    (do ((current last (vop-prev current)))
	((null current))
      (when (eq (vop-info-name (vop-info current)) name)
	(return-from reverse-find-vop current)))))

;;; Save-Complex-Writer-TN  --  Internal
;;;
;;; For TNs that have other than one writer, we save the TN before each
;;; call.  If a local call (MOVE-ARGS is :LOCAL-CALL), then we scan back
;;; for the ALLOCATE-FRAME VOP, and emit the save there.  This is necessary
;;; because in a self-recursive local call, the registers holding the
;;; current arguments may get trashed by setting up the call arguments.
;;; The ALLOCATE-FRAME VOP marks a place at which the values are known to
;;; be good.
;;;
(defun save-complex-writer-tn (tn vop)
  (let ((save (or (tn-save-tn tn)
		  (pack-save-tn tn)))
	(node (vop-node vop))
	(block (vop-block vop))
	(next (vop-next vop)))
    (when (eq (tn-kind save) :specified-save)
      (setf (tn-kind save) :save))
    (assert (eq (tn-kind save) :save))
    (emit-operand-load node block tn save
		       (if (eq (vop-info-move-args (vop-info vop))
			       :local-call)
			   (reverse-find-vop 'allocate-frame vop)
			   vop))
    (emit-operand-load node block save tn next)))

;;; FIND-SINGLE-WRITER  --  Internal
;;;
;;; Return a VOP after which is an o.k. place to save the value of TN.  For
;;; correctness, it is only required that this location be after any
;;; possible write and before any possible restore location.
;;;
;;; In practice, we return the unique writer VOP, but give up if the TN is
;;; ever read by a VOP with MOVE-ARGS :LOCAL-CALL.  This prevents us from
;;; being confused by non-tail local calls.
;;;
;;; When looking for writes, we have to ignore uses of MOVE-OPERAND, since they
;;; will correspond to restores that we have already done.
;;;
(defun find-single-writer (tn)
  (declare (type tn tn))
  (do ((write (tn-writes tn) (tn-ref-next write))
       (res nil))
      ((null write)
       (when (and res
		  (do ((read (tn-reads tn) (tn-ref-next read)))
		      ((not read) t)
		    (when (eq (vop-info-move-args
			       (vop-info
				(tn-ref-vop read)))
			      :local-call)
		      (return nil))))
	 (tn-ref-vop res)))

    (unless (eq (vop-info-name (vop-info (tn-ref-vop write)))
		'move-operand)
      (when res (return nil))
      (setq res write))))

;;; Save-Single-Writer-TN  --  Internal
;;;
;;; Try to save TN at a single location.  If we succeed, return T,
;;; otherwise NIL.
;;;
(defun save-single-writer-tn (tn)
  (declare (type tn tn))
  (let* ((old-save (tn-save-tn tn))
	 (save (or old-save (pack-save-tn tn)))
	 (writer (find-single-writer tn)))
    (when (and writer
	       (or (not old-save)
		   (eq (tn-kind old-save) :specified-save)))
      (emit-operand-load (vop-node writer) (vop-block writer)
			 tn save (vop-next writer))
      (setf (tn-kind save) :save-once)
      t)))

;;; RESTORE-SINGLE-WRITER-TN  --  Internal
;;;
;;; Restore a TN with a :SAVE-ONCE save TN.
;;;
(defun restore-single-writer-tn (tn vop)
  (declare (type tn) (type vop vop))
  (let ((save (tn-save-tn tn)))
    (assert (eq (tn-kind save) :save-once))
    (emit-operand-load (vop-node vop) (vop-block vop) save tn (vop-next vop)))
  (undefined-value))

;;; BASIC-SAVE-TN  --  Internal
;;;
;;; Save a single TN that needs to be saved, choosing save-once if
;;; appropriate.  This is also called by SPILL-AND-PACK-LOAD-TN.
;;;
(defun basic-save-tn (tn vop)
  (declare (type tn tn) (type vop vop))
  (let ((save (tn-save-tn tn)))
    (cond ((and save (eq (tn-kind save) :save-once))
	   (restore-single-writer-tn tn vop))
	  ((save-single-writer-tn tn)
	   (restore-single-writer-tn tn vop))
	  (t
	   (save-complex-writer-tn tn vop))))
  (undefined-value))

;;; Emit-Saves  --  Internal
;;;
;;; Scan over the VOPs in Block, emiting saving code for TNs noted in the
;;; codegen info that are packed into saved SCs.
;;;
(defun emit-saves (block)
  (declare (type ir2-block block))
  (do ((vop (ir2-block-start-vop block) (vop-next vop)))
      ((null vop))
    (when (eq (vop-info-save-p (vop-info vop)) t)
      (do-live-tns (tn (vop-save-set vop) block)
	(when (and (sc-save-p (tn-sc tn))
		   (not (eq (tn-kind tn) :component)))
	  (basic-save-tn tn vop)))))

  (undefined-value))


;;;; Optimized saving.

;;; SAVE-IF-NECESSARY  --  Internal
;;;
;;; Save TN if it isn't a single-writer TN that has already been saved.  If
;;; multi-write, we insert the save Before the specified VOP.  Context is a
;;; VOP used to tell which node/block to use for the new VOP.
;;;
(defun save-if-necessary (tn before context)
  (declare (type tn tn) (type (or vop null) before) (type vop context))
  (let ((save (tn-save-tn tn)))
    (when (eq (tn-kind save) :specified-save)
      (setf (tn-kind save) :save))
    (assert (member (tn-kind save) '(:save :save-once)))
    (unless (eq (tn-kind save) :save-once)
      (or (save-single-writer-tn tn)
	  (emit-operand-load (vop-node context) (vop-block context)
			     tn save before))))
  (undefined-value))

;;; RESTORE-TN  --  Internal
;;;
;;; Load the TN from its save location, allocating one if necessary.  The
;;; load is inserted Before the specifier VOP.  Context is a VOP used to
;;; tell which node/block to use for the new VOP.
;;;
(defun restore-tn (tn before context)
  (declare (type tn tn) (type (or vop null) before) (type vop context))
  (let ((save (or (tn-save-tn tn) (pack-save-tn tn))))
    (emit-operand-load (vop-node context) (vop-block context)
		       save tn before))
  (undefined-value))

(eval-when (compile eval)

;;; SAVE-NOTE-READ  --  Internal
;;;
;;; Do stuff to note a read of TN, for OPTIMIZED-EMIT-SAVES-BLOCK.
;;;
(defmacro save-note-read (tn)
  `(let* ((tn ,tn)
	  (num (tn-number tn)))
     (when (and (sc-save-p (tn-sc tn))
		(zerop (sbit restores num))
		(not (eq (tn-kind tn) :component)))
       (setf (sbit restores num) 1)
       (push tn restores-list))))

); Eval-When (Compile Eval)

;;; OPTIMIZED-EMIT-SAVES-BLOCK  --  Internal
;;;
;;; Start scanning backward at the end of Block, looking which TNs are live
;;; and looking for places where we have to save.  We manipulate two sets:
;;; SAVES and RESTORES.
;;;
;;; SAVES is a set of all the TNs that have to be saved because they are
;;; restored after some call.  We normally delay saving until the beginning
;;; of the block, but we must save immediately if we see a write of the
;;; saved TN.  We also immediately save all TNs and exit when we see a
;;; NOTE-ENVIRONMENT-START VOP, since saves can't be done before the
;;; environment is properly initialized.
;;;
;;; RESTORES is a set of all the TNs read (and not written) between here
;;; and the next call, i.e. the set of TNs that must be restored when we
;;; reach the next (earlier) call VOP.  Unlike SAVES, this set is cleared
;;; when we do the restoring after a call.  Any TNs that were in RESTORES
;;; are moved into SAVES to ensure that they are saved at some point.
;;;
;;; SAVES and RESTORES are represented using both a list and a bit-vector
;;; so that we can quickly iterate and test for membership.  The incoming
;;; Saves and Restores args are used for computing these sets (the initial
;;; contents are ignored.)
;;;
;;; When we hit a VOP with :COMPUTE-ONLY Save-P (an internal error
;;; location), we pretend that all live TNs were read, unless (= speed 3),
;;; in which case we mark all the TNs that are live but not restored as
;;; spilled.
;;;
(defun optimized-emit-saves-block (block saves restores)
  (declare (type ir2-block block) (type simple-bit-vector saves restores))
  (let ((1block (ir2-block-block block))
	(saves-list ())
	(restores-list ())
	(skipping nil))
    (declare (list saves-list restores-list))
    (clear-bit-vector saves)
    (clear-bit-vector restores)
    (do-live-tns (tn (ir2-block-live-in block) block)
      (when (and (sc-save-p (tn-sc tn))
		 (not (eq (tn-kind tn) :component)))
	(let ((num (tn-number tn)))
	  (setf (sbit restores num) 1)
	  (push tn restores-list))))

    (do ((block block (ir2-block-prev block))
	 (prev nil block))
	((not (eq (ir2-block-block block) 1block))
	 (assert (not skipping))
	 (dolist (save saves-list)
	   (let ((start (ir2-block-start-vop prev)))
	     (save-if-necessary save start start)))
	 prev)
      (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
	  ((null vop))
	(let ((info (vop-info vop)))
	  (case (vop-info-name info)
	    (allocate-frame
	     (assert skipping)
	     (setq skipping nil))
	    (note-environment-start
	     (assert (not skipping))
	     (dolist (save saves-list)
	       (save-if-necessary save (vop-next vop) vop))
	     (return-from optimized-emit-saves-block block)))

	  (unless skipping
	    (do ((write (vop-results vop) (tn-ref-across write)))
		((null write))
	      (let* ((tn (tn-ref-tn write))
		     (num (tn-number tn)))
		(unless (zerop (sbit restores num))
		  (setf (sbit restores num) 0)
		  (setq restores-list
			(delete tn restores-list :test #'eq)))
		(unless (zerop (sbit saves num))
		  (setf (sbit saves num) 0)
		  (save-if-necessary tn (vop-next vop) vop)
		  (setq saves-list
			(delete tn saves-list :test #'eq))))))

	  (case (vop-info-save-p info)
	    ((t)
	     (dolist (tn restores-list)
	       (restore-tn tn (vop-next vop) vop)
	       (let ((num (tn-number tn)))
		 (when (zerop (sbit saves num))
		   (push tn saves-list)
		   (setf (sbit saves num) 1))))
	     (setq restores-list nil)
	     (clear-bit-vector restores))
	    (:compute-only
	     (cond ((policy (vop-node vop) (= speed 3))
		    (do-live-tns (tn (vop-save-set vop) block)
		      (when (zerop (sbit restores (tn-number tn)))
			(note-spilled-tn tn vop))))
		   (t
		    (do-live-tns (tn (vop-save-set vop) block)
		      (save-note-read tn))))))

	  (if (eq (vop-info-move-args info) :local-call)
	      (setq skipping t)
	      (do ((read (vop-args vop) (tn-ref-across read)))
		  ((null read))
		(save-note-read (tn-ref-tn read)))))))))

;;; OPTIMIZED-EMIT-SAVES  --  Internal
;;;
;;; Like EMIT-SAVES, only different.  We avoid redundant saving within the
;;; block, and don't restore values that aren't used before the next call.
;;; This function is just the top-level loop over the blocks in the
;;; component, which locates blocks that need saving done.
;;;
(defun optimized-emit-saves (component)
  (declare (type component component))
  (let* ((gtn-count (1+ (ir2-component-global-tn-counter
			 (component-info component))))
	 (saves (make-array gtn-count :element-type 'bit))
	 (restores (make-array gtn-count :element-type 'bit))
	 (block (ir2-block-prev (block-info (component-tail component))))
	 (head (block-info (component-head component))))
    (loop
      (when (eq block head) (return))
      (when (do ((vop (ir2-block-start-vop block) (vop-next vop)))
		((null vop) nil)
	      (when (eq (vop-info-save-p (vop-info vop)) t)
		(return t)))
	(setq block (optimized-emit-saves-block block saves restores)))
      (setq block (ir2-block-prev block)))))

;;; ASSIGN-TN-COSTS  --  Internal
;;;
;;; Iterate over the normal TNs, finding the cost of packing on the stack
;;; in units of the number of references.  We count all references as +1,
;;; and subtract out REGISTER-SAVE-PENALTY for each place where we would
;;; have to save a register.
;;;
(defun assign-tn-costs (component)
  (do-ir2-blocks (block component)
    (do ((vop (ir2-block-start-vop block) (vop-next vop)))
	((null vop))
      (when (eq (vop-info-save-p (vop-info vop)) t)
	(do-live-tns (tn (vop-save-set vop) block)
	  (decf (tn-cost tn)
		(the index (backend-register-save-penalty *backend*)))))))

  (do ((tn (ir2-component-normal-tns (component-info component))
	   (tn-next tn)))
      ((null tn))
    (let ((cost (tn-cost tn)))
      (declare (fixnum cost))
      (do ((ref (tn-reads tn) (tn-ref-next ref)))
	  ((null ref))
	(incf cost))
      (do ((ref (tn-writes tn) (tn-ref-next ref)))
	  ((null ref))
	(incf cost))
      (setf (tn-cost tn) cost))))

(declaim (end-block))


;;;; Load TN packing.

(declaim (start-block pack-load-tns load-tn-conflicts-in-sc))

;;; These variables indicate the last location at which we computed the
;;; Live-TNs.  They hold the Block and VOP values that were passed to
;;; Compute-Live-TNs.
;;;
(defvar *live-block*)
(defvar *live-vop*)

;;; If we unpack some TNs, then we mark all affected blocks by sticking them in
;;; this hash-table.  This is initially null.  We create the hashtable if we do
;;; any unpacking.
;;;
(defvar *repack-blocks*)
(declaim (type (or hash-table null) *repack-blocks*))

;;; Init-Live-TNs  --  Internal
;;;
;;; Set the Live-TNs vectors in all :Finite SBs to represent the TNs live
;;; at the end of Block.
;;;
(defun init-live-tns (block)
  (dolist (sb (backend-sb-list *backend*))
    (when (eq (sb-kind sb) :finite)
      (fill (finite-sb-live-tns sb) nil)))

  (do-live-tns (tn (ir2-block-live-in block) block)
    (let* ((sc (tn-sc tn))
	   (sb (sc-sb sc)))
      (when (eq (sb-kind sb) :finite)
	(do ((offset (tn-offset tn) (1+ offset))
	     (end (+ (tn-offset tn) (sc-element-size sc))))
	    ((= offset end))
	  (declare (type index offset end))
	  (setf (svref (finite-sb-live-tns sb) offset) tn)))))

  (setq *live-block* block)
  (setq *live-vop* (ir2-block-last-vop block))

  (undefined-value))

;;; Compute-Live-TNs  --  Internal
;;;
;;; Set the Live-TNs in :Finite SBs to represent the TNs live immediately
;;; after the evaluation of VOP in Block, excluding results of the VOP.  If
;;; VOP is null, then compute the live TNs at the beginning of the block.
;;; Sequential calls on the same block must be in reverse VOP order.
;;;
(defun compute-live-tns (block vop)
  (declare (type ir2-block block) (type vop vop))
  (unless (eq block *live-block*)
    (init-live-tns block))

  (do ((current *live-vop* (vop-prev current)))
      ((eq current vop)
       (do ((res (vop-results vop) (tn-ref-across res)))
	   ((null res))
	 (let* ((tn (tn-ref-tn res))
		(sc (tn-sc tn))
		(sb (sc-sb sc)))
	   (when (eq (sb-kind sb) :finite)
	     (do ((offset (tn-offset tn) (1+ offset))
		  (end (+ (tn-offset tn) (sc-element-size sc))))
		 ((= offset end))
	       (declare (type index offset end))
	       (setf (svref (finite-sb-live-tns sb) offset) nil))))))
    (do ((ref (vop-refs current) (tn-ref-next-ref ref)))
	((null ref))
      (let ((ltn (tn-ref-load-tn ref)))
	(when ltn
	  (let* ((sc (tn-sc ltn))
		 (sb (sc-sb sc)))
	    (when (eq (sb-kind sb) :finite)
	      (let ((tns (finite-sb-live-tns sb)))
		(do ((offset (tn-offset ltn) (1+ offset))
		     (end (+ (tn-offset ltn) (sc-element-size sc))))
		    ((= offset end))
		  (declare (type index offset end))
		  (assert (null (svref tns offset)))))))))

      (let* ((tn (tn-ref-tn ref))
	     (sc (tn-sc tn))
	     (sb (sc-sb sc)))
	(when (eq (sb-kind sb) :finite)
	  (let ((tns (finite-sb-live-tns sb)))
	    (do ((offset (tn-offset tn) (1+ offset))
		 (end (+ (tn-offset tn) (sc-element-size sc))))
		((= offset end))
	      (declare (type index offset end))
	      (if (tn-ref-write-p ref)
		  (setf (svref tns offset) nil)
		  (let ((old (svref tns offset)))
		    (assert (or (null old) (eq old tn)) (old tn))
		    (setf (svref tns offset) tn)))))))))

  (setq *live-vop* vop)
  (undefined-value))

;;; LOAD-TN-OFFSET-CONFLICTS-IN-SB  --  Internal
;;;
;;; Kind of like Offset-Conflicts-In-SB, except that it uses the VOP refs
;;; to determine whether a Load-TN for OP could be packed in the specified
;;; location, disregarding conflicts with TNs not referenced by this VOP.
;;; There is a conflict if either: 1] The reference is a result, and the
;;; same location is either: -- Used by some other result.  -- Used in any
;;; way after the reference (exclusive).  2] The reference is an argument,
;;; and the same location is either: -- Used by some other argument.  --
;;; Used in any way before the reference (exclusive).
;;;
;;; In 1 (and 2) above, the first bullet corresponds to result-result (and
;;; argument-argument) conflicts.  We need this case because there aren't
;;; any TN-REFs to represent the implicit reading of results or writing of
;;; arguments.
;;;
;;; The second bullet corresponds conflicts with temporaries or between
;;; arguments and results.
;;;
;;; We consider both the TN-REF-TN and the TN-REF-LOAD-TN (if any) to be
;;; referenced simultaneously and in the same way.  This causes load-TNs to
;;; appear live to the beginning (or end) of the VOP, as appropriate.
;;;
;;; We return a conflicting TN if there is a conflict.
;;;
(defun load-tn-offset-conflicts-in-sb (op sb offset)
  (declare (type tn-ref op) (type finite-sb sb) (type index offset))
  (assert (eq (sb-kind sb) :finite))
  (let ((vop (tn-ref-vop op)))
    (labels ((tn-overlaps (tn)
	       (let ((sc (tn-sc tn))
		     (tn-offset (tn-offset tn)))
		 (when (and (eq (sc-sb sc) sb)
			    (<= tn-offset offset)
			    (< offset
			       (the index
				    (+ tn-offset (sc-element-size sc)))))
		   tn)))
	     (same (ref)
	       (let ((tn (tn-ref-tn ref))
		     (ltn (tn-ref-load-tn ref)))
		 (or (tn-overlaps tn)
		     (and ltn (tn-overlaps ltn)))))
	     (is-op (ops)
	       (do ((ops ops (tn-ref-across ops)))
		   ((null ops) nil)
		 (let ((found (same ops)))
		   (when (and found (not (eq ops op)))
		     (return found)))))
	     (is-ref (refs end)
	       (do ((refs refs (tn-ref-next-ref refs)))
		   ((eq refs end) nil)
		 (let ((found (same refs)))
		 (when found (return found))))))
      (declare (inline is-op is-ref tn-overlaps))
      (if (tn-ref-write-p op)
	  (or (is-op (vop-results vop))
	      (is-ref (vop-refs vop) op))
	  (or (is-op (vop-args vop))
	      (is-ref (tn-ref-next-ref op) nil))))))

;;; LOAD-TN-CONFLICTS-IN-SC  --  Internal
;;;
;;; Iterate over all the elements in the SB that would be allocated by
;;; allocating a TN in SC at Offset, checking for conflict with load-TNs or
;;; other TNs (live in the LIVE-TNS, which must be set up.)  We also return
;;; true if there aren't enough locations after Offset to hold a TN in SC.
;;; If Ignore-Live is true, then we ignore the live-TNs, considering only
;;; references within Op's VOP.
;;;
;;; We return a conflicting TN, or :OVERFLOW if the TN won't fit.
;;;
(defun load-tn-conflicts-in-sc (op sc offset ignore-live)
  (let* ((sb (sc-sb sc))
	 (size (finite-sb-current-size sb)))
    (do ((i offset (1+ i))
	 (end (+ offset (sc-element-size sc))))
	((= i end) nil)
      (declare (type index i end))
      (let ((res (or (when (>= i size) :overflow)
		     (and (not ignore-live)
			  (svref (finite-sb-live-tns sb) i))
		     (load-tn-offset-conflicts-in-sb op sb i))))
	(when res (return res))))))

;;; Find-Load-TN-Target  --  Internal
;;;
;;; If a load-TN for Op is targeted to a legal location in SC, then return
;;; the offset, otherwise return NIL.  We see if the target of the operand
;;; is packed, and try that location.  There isn't any need to chain down
;;; the target path, since everything is packed now.
;;;
;;; We require the target to be in SC (and not merely to overlap with SC).
;;; This prevents SC information from being lost in load TNs (we won't pack
;;; a load TN in ANY-REG when it is targeted to a DESCRIPTOR-REG.)  This
;;; shouldn't hurt the code as long as all relevant overlapping SCs are
;;; allowed in the operand SC restriction.
;;;
(defun find-load-tn-target (op sc)
  (declare (inline member))
  (let ((target (tn-ref-target op)))
    (when target
      (let* ((tn (tn-ref-tn target))
	     (loc (tn-offset tn)))
	(if (and (eq (tn-sc tn) sc)
		 (member (the index loc) (sc-locations sc))
		 (not (load-tn-conflicts-in-sc op sc loc nil)))
	    loc
	    nil)))))

;;; Select-Load-Tn-Location  --  Internal
;;;
;;; Select a legal location for a load TN for Op in SC.  We just iterate
;;; over the SC's locations.  If we can't find a legal location, return
;;; NIL.
;;;
(defun select-load-tn-location (op sc)
  (declare (type tn-ref op) (type sc sc))

  ;; Check any target location first.
  (let ((target (tn-ref-target op)))
    (when target
      (let* ((tn (tn-ref-tn target))
	     (loc (tn-offset tn)))
	(when (and (eq (sc-sb sc) (sc-sb (tn-sc tn)))
		   (member (the index loc) (sc-locations sc))
		   (not (load-tn-conflicts-in-sc op sc loc nil)))
	      (return-from select-load-tn-location loc)))))

  (dolist (loc (sc-locations sc) nil)
    (unless (load-tn-conflicts-in-sc op sc loc nil)
      (return loc))))

(defevent unpack-tn "Unpacked a TN to satisfy operand SC restriction.")

;;; UNPACK-TN  --  Internal
;;;
;;; Make TN's location the same as for its save TN (allocating a save TN if
;;; necessary.)  Delete any save/restore code that has been emitted thus
;;; far.  Mark all blocks containing references as needing to be repacked.
;;;
(defun unpack-tn (tn)
  (event unpack-tn)
  (let ((stn (or (tn-save-tn tn)
		 (pack-save-tn tn))))
    (setf (tn-sc tn) (tn-sc stn))
    (setf (tn-offset tn) (tn-offset stn))
    (flet ((zot (refs)
	     (do ((ref refs (tn-ref-next ref)))
		 ((null ref))
	       (let ((vop (tn-ref-vop ref)))
		 (if (eq (vop-info-name (vop-info vop)) 'move-operand)
		     (delete-vop vop)
		     (setf (gethash (vop-block vop) *repack-blocks*) t))))))
      (zot (tn-reads tn))
      (zot (tn-writes tn))))

  (undefined-value))

(defevent unpack-fallback "Unpacked some random operand TN.")

;;; UNPACK-FOR-LOAD-TN  --  Internal
;;;
;;; Called by Pack-Load-TN where there isn't any location free that we can
;;; pack into.  What we do is move some live TN in one of the specified SCs
;;; to memory, then mark this block all blocks that reference the TN as
;;; needing repacking.  If we suceed, we throw to UNPACKED-TN.  If we fail,
;;; we return NIL.
;;;
;;; We can unpack any live TN that appears in the NORMAL-TNs list (isn't wired
;;; or restricted.)  We prefer to unpack TNs that are not used by the VOP.  If
;;; we can't find any such TN, then we unpack some random argument or result
;;; TN.  The only way we can fail is if all locations in SC are used by
;;; load-TNs or temporaries in VOP.
;;;
(defun unpack-for-load-tn (sc op)
  (declare (type sc sc) (type tn-ref op))
  (let ((sb (sc-sb sc))
	(normal-tns (ir2-component-normal-tns
		     (component-info *compile-component*)))
	(node (vop-node (tn-ref-vop op)))
	(fallback nil))
    (flet ((unpack-em (victims)
	     (unless *repack-blocks*
	       (setq *repack-blocks* (make-hash-table :test #'eq)))
	     (setf (gethash (vop-block (tn-ref-vop op)) *repack-blocks*) t)
	     (dolist (victim victims)
	       (event unpack-tn node)
	       (unpack-tn victim))
	     (throw 'unpacked-tn nil)))
      (dolist (loc (sc-locations sc))
	(declare (type index loc))
	(block SKIP
	  (collect ((victims nil adjoin))
	    (do ((i loc (1+ i))
		 (end (+ loc (sc-element-size sc))))
		((= i end))
	      (declare (type index i end))
	      (let ((victim (svref (finite-sb-live-tns sb) i)))
		(when victim
		  (unless (find-in #'tn-next victim normal-tns)
		    (return-from SKIP))
		  (victims victim))))

	    (let ((conf (load-tn-conflicts-in-sc op sc loc t)))
	      (cond ((not conf)
		     (unpack-em (victims)))
		    ((eq conf :overflow))
		    ((not fallback)
		     (cond ((find conf (victims))
			    (setq fallback (victims)))
			   ((find-in #'tn-next conf normal-tns)
			    (setq fallback (list conf))))))))))

      (when fallback
	(event unpack-fallback node)
	(unpack-em fallback))))

  nil)

;;; Pack-Load-TN  --  Internal
;;;
;;; Try to pack a load TN in the SCs indicated by Load-SCs.  If we run out
;;; of SCs, then we unpack some TN and try again.  We return the packed
;;; load TN.
;;;
;;; Note: we allow a Load-TN to be packed in the target location even if that
;;; location is in a SC not allowed by the primitive type.  (The SC must still
;;; be allowed by the operand restriction.)  This makes move VOPs more
;;; efficient, since we won't do a move from the stack into a non-descriptor
;;; any-reg though a descriptor argument load-TN.  This does give targeting
;;; some real semantics, making it not a pure advisory to pack.  It allows pack
;;; to do some packing it wouldn't have done before.
;;;
(defun pack-load-tn (load-scs op)
  (declare (type sc-vector load-scs) (type tn-ref op))
  (let ((vop (tn-ref-vop op)))
    (compute-live-tns (vop-block vop) vop))

  (let* ((tn (tn-ref-tn op))
	 (ptype (tn-primitive-type tn))
	 (scs (svref load-scs (sc-number (tn-sc tn)))))
    (let ((current-scs scs)
	  (allowed ()))
      (loop
	(cond
	 ((null current-scs)
	  (unless allowed
	    (no-load-scs-allowed-by-primitive-type-error op))
	  (dolist (sc allowed)
	    (unpack-for-load-tn sc op))
	  (failed-to-pack-load-tn-error allowed op))
	(t
	 (let* ((sc (svref (backend-sc-numbers *backend*) (pop current-scs)))
		(target (find-load-tn-target op sc)))
	   (when (or target (sc-allowed-by-primitive-type sc ptype))
	     (let ((loc (or target
			    (select-load-tn-location op sc))))
	       (when loc
		 (let ((res (make-tn 0 :load nil sc)))
		   (setf (tn-offset res) loc)
		   (return res))))
	     (push sc allowed)))))))))

;;; Check-Operand-Restrictions  --  Internal
;;;
;;; Scan a list of load-SCs vectors and a list of TN-Refs threaded by
;;; TN-Ref-Across.  When we find a reference whose TN doesn't satisfy the
;;; restriction, we pack a Load-TN and load the operand into it.  If a
;;; load-tn has already been allocated, we can assume that the restriction
;;; is satisfied.
;;;
(proclaim '(inline check-operand-restrictions))
(defun check-operand-restrictions (scs ops)
  (declare (list scs) (type (or tn-ref null) ops))

  ;; Check the targeted operands first.
  (do ((scs scs (cdr scs))
       (op ops (tn-ref-across op)))
      ((null scs))
    (let ((target (tn-ref-target op)))
      (when target
	(let* ((load-tn (tn-ref-load-tn op))
	       (load-scs (svref (car scs)
				(sc-number
				 (tn-sc (or load-tn (tn-ref-tn op)))))))
	  (if load-tn
	      (assert (eq load-scs t))
	      (unless (eq load-scs t)
		(setf (tn-ref-load-tn op)
		      (pack-load-tn (car scs) op))))))))

  (do ((scs scs (cdr scs))
       (op ops (tn-ref-across op)))
      ((null scs))
      (let ((target (tn-ref-target op)))
	(unless target
	  (let* ((load-tn (tn-ref-load-tn op))
		 (load-scs (svref (car scs)
				  (sc-number
				   (tn-sc (or load-tn (tn-ref-tn op)))))))
	    (if load-tn
		(assert (eq load-scs t))
		(unless (eq load-scs t)
		  (setf (tn-ref-load-tn op)
			(pack-load-tn (car scs) op))))))))

  (undefined-value))

;;; Pack-Load-TNs  --  Internal
;;;
;;; Scan the VOPs in Block, looking for operands whose SC restrictions
;;; aren't statisfied.  We do the results first, since they are evaluated
;;; later, and our conflict analysis is a backward scan.
;;;
(defun pack-load-tns (block)
  (catch 'unpacked-tn
    (let ((*live-block* nil)
	  (*live-vop* nil))
      (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
	  ((null vop))
	(let ((info (vop-info vop)))
	  (check-operand-restrictions (vop-info-result-load-scs info)
				      (vop-results vop))
	  (check-operand-restrictions (vop-info-arg-load-scs info)
				      (vop-args vop))))))
  (undefined-value))


;;;; Location-selection, targeting & pack interface.

(declaim (start-block pack pack-tn target-if-desirable))


;;;; Targeting.

;;; Target-If-Desirable  --  Internal
;;;
;;; Link the TN-Refs Read and Write together using the TN-Ref-Target when
;;; this seems like a good idea.  Currently we always do, as this increases
;;; the sucess of load-TN targeting.
;;;
(defun target-if-desirable (read write)
  (declare (type tn-ref read write))
  (setf (tn-ref-target read) write)
  (setf (tn-ref-target write) read))

;;; Check-OK-Target  --  Internal
;;;
;;; If TN can be packed into SC so as to honor a preference to Target, then
;;; return the offset to pack at, otherwise return NIL.  Target must be
;;; already packed.  We can honor a preference if: -- Target's location is
;;; in SC's locations.  -- The element sizes of the two SCs are the same.
;;; -- TN doesn't conflict with target's location.
;;;
(defun check-ok-target (target tn sc)
  (declare (type tn target tn) (type sc sc) (inline member))
  (let* ((loc (tn-offset target))
	 (target-sc (tn-sc target))
	 (target-sb (sc-sb target-sc)))
    (declare (type index loc))
    (if (and (eq target-sb (sc-sb sc))
	     (or (eq (sb-kind target-sb) :unbounded)
		 (member loc (sc-locations sc)))
	     (= (sc-element-size target-sc) (sc-element-size sc))
	     (not (conflicts-in-sc tn sc loc))
	     (zerop (mod loc (sc-alignment sc))))
	loc
	nil)))

;;; Find-OK-Target-Offset  --  Internal
;;;
;;; Scan along the target path from TN, looking at readers or writers.
;;; When we find a packed TN, return Check-OK-Target of that TN.  If there
;;; is no target, or if the TN has multiple readers (writers), then we
;;; return NIL.  We also always return NIL after 10 iterations to get
;;; around potential circularity problems.
;;;
(macrolet ((frob (slot)
	     `(let ((count 10)
		    (current tn))
		(declare (type index count))
		(loop
		  (let ((refs (,slot current)))
		    (unless (and (plusp count) refs (not (tn-ref-next refs)))
		      (return nil))
		    (let ((target (tn-ref-target refs)))
		      (unless target (return nil))
		      (setq current (tn-ref-tn target))
		      (when (tn-offset current)
			(return (check-ok-target current tn sc)))
		      (decf count)))))))
  (defun find-ok-target-offset (tn sc)
    (declare (type tn tn) (type sc sc))
    (or (frob tn-reads)
	(frob tn-writes))))


;;;; Location selection.

;;; Select-Location  --  Internal
;;;
;;; Select some location for TN in SC, returning the offset if we succeed,
;;; and NIL if we fail.  We start scanning at the Last-Offset in an attempt
;;; to distribute the TNs across all storage.
;;;
;;; We call Offset-Conflicts-In-SB directly, rather than using
;;; Conflicts-In-SC.  This allows us to be more efficient in packing
;;; multi-location TNs: we don't have to multiply the number of tests by
;;; the TN size.  This falls out naturally, since we have to be aware of TN
;;; size anyway so that we don't call Conflicts-In-SC on a bogus offset.
;;;
;;; We give up on finding a location after our current pointer has wrapped
;;; twice.  This will result in testing some locations twice in the case that
;;; we fail, but is simpler than trying to figure out the soonest failure
;;; point.
;;;
;;; We also give up without bothering to wrap if the current size isn't large
;;; enough to hold a single element of element-size without bothering to wrap.
;;; If it doesn't fit this iteration, it won't fit next.
;;;
;;; ### Note that we actually try to pack as many consecutive TNs as possible
;;; in the same location, since we start scanning at the same offset that the
;;; last TN was successfully packed in.  This is a weakening of the scattering
;;; hueristic that was put in to prevent restricted VOP temps from hogging all
;;; of the registers.  This way, all of these temps probably end up in one
;;; register.
;;;
(defun select-location (tn sc &optional use-reserved-locs)
  (declare (type tn tn) (type sc sc) (inline member))
  (let* ((sb (sc-sb sc))
	 (element-size (sc-element-size sc))
	 (alignment (sc-alignment sc))
	 (align-mask (1- alignment))
	 (size (finite-sb-current-size sb))
	 (start-offset (finite-sb-last-offset sb)))
    (let ((current-start
	   (logandc2 (the index (+ start-offset align-mask)) align-mask))
	  (wrap-p nil))
      (declare (type index current-start))
      (loop
	(when (> (+ current-start element-size) size)
	  (cond ((or wrap-p (> element-size size))
		 (return nil))
		(t
		 (setq current-start 0)
		 (setq wrap-p t))))

	(if (or (eq (sb-kind sb) :unbounded)
		(and (member current-start (sc-locations sc))
		     (or use-reserved-locs
			 (not (member current-start
				      (sc-reserve-locations sc))))))
	    (dotimes (i element-size
			(return-from select-location current-start))
	      (declare (type index i))
	      (let ((offset (+ current-start i)))
		(when (offset-conflicts-in-sb tn sb offset)
		  (setq current-start
			(logandc2 (the index (+ (the index (1+ offset))
						align-mask))
				  align-mask))
		  (return))))
	    (incf current-start alignment))))))

;;; Original-TN  --  Internal
;;;
;;; If a save TN, return the saved TN, otherwise return TN.  Useful for
;;; getting the conflicts of a TN that might be a save TN.
;;;
(defun original-tn (tn)
  (declare (type tn tn))
  (if (member (tn-kind tn) '(:save :save-once :specified-save))
      (tn-save-tn tn)
      tn))


;;;; Pack interface.

;;; Pack-TN  --  Internal
;;;
;;; Attempt to pack TN in all possible SCs, first in the SC chosen by
;;; representation selection, then in the alternate SCs in the order they
;;; were specified in the SC definition.  If the TN-COST is negative, then
;;; we don't attempt to pack in SCs that must be saved.  If Restricted,
;;; then we can only pack in TN-SC, not in any Alternate-SCs.
;;;
;;; If we are attempting to pack in the SC of the save TN for a TN with a
;;; :SPECIFIED-SAVE TN, then we pack in that location, instead of
;;; allocating a new stack location.
;;;
(defun pack-tn (tn restricted)
  (declare (type tn tn))
  (let* ((original (original-tn tn))
	 (fsc (tn-sc tn))
	 (alternates (unless restricted (sc-alternate-scs fsc)))
	 (save (tn-save-tn tn))
	 (specified-save-sc
	  (when (and save
		     (eq (tn-kind save) :specified-save))
	    (tn-sc save))))

    (do ((sc fsc (pop alternates)))
	((null sc)
	 (failed-to-pack-error tn restricted))
      (when (eq sc specified-save-sc)
	(unless (tn-offset save)
	  (pack-tn save nil))
	(setf (tn-offset tn) (tn-offset save))
	(setf (tn-sc tn) (tn-sc save))
	(return))
      (when (or restricted
		(not (and (minusp (tn-cost tn)) (sc-save-p sc))))
	(let ((loc (or (find-ok-target-offset original sc)
		       (select-location original sc)
		       (and restricted
			    (select-location original sc t))
		       (when (eq (sb-kind (sc-sb sc)) :unbounded)
			 (grow-sc sc)
			 (or (select-location original sc)
			     (error "Failed to pack after growing SC?"))))))
	  (when loc
	    (add-location-conflicts original sc loc)
	    (setf (tn-sc tn) sc)
	    (setf (tn-offset tn) loc)
	    (return))))))

  (undefined-value))

;;; Pack-Wired-TN  --  Internal
;;;
;;; Pack a wired TN, checking that the offset is in bounds for the SB, and
;;; that the TN doesn't conflict with some other TN already packed in that
;;; location.  If the TN is wired to a location beyond the end of a
;;; :Unbounded SB, then grow the SB enough to hold the TN.
;;;
;;; ### Checking for conflicts is disabled for :SPECIFIED-SAVE TNs.  This is
;;; kind of a hack to make specifying wired stack save locations for local call
;;; arguments (such as OLD-FP) work, since the caller and callee OLD-FP save
;;; locations may conflict when the save locations don't really (due to being
;;; in different frames.)
;;;
(defun pack-wired-tn (tn)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
	 (sb (sc-sb sc))
	 (offset (tn-offset tn))
	 (end (+ offset (sc-element-size sc)))
	 (original (original-tn tn)))
    (when (> end (finite-sb-current-size sb))
      (unless (eq (sb-kind sb) :unbounded)
	(error "~S wired to a location that is out of bounds." tn))
      (grow-sc sc end))

    ;; FIX why implementation dependent here?
    ;;
    ;; For non x86 ports the presence of a save-tn associated with a
    ;; tn is used to identify the old-fp and return-pc tns. It depends
    ;; on the old-fp and return-pc being passed in registers.
    (unless (backend-featurep :x86)
      (when (and (not (eq (tn-kind tn) :specified-save))
		 (conflicts-in-sc original sc offset))
	(error "~S wired to a location that it conflicts with." tn)))

    ;; Use the above check, but only print a verbose warning. Helpful
    ;; for debugging the x86 port.
    #+x86-debug-pack-wired-tn
    (when (and (not (eq (tn-kind tn) :specified-save))
	       (conflicts-in-sc original sc offset))
	  (format t "~&* Pack-wired-tn possible conflict:~%  ~
                     tn: ~s; tn-kind: ~s~%  ~
                     sc: ~s~%  ~
                     sb: ~s; sb-name: ~s; sb-kind: ~s~%  ~
                     offset: ~s; end: ~s~%  ~
                     original ~s~%  ~
                     tn-save-tn: ~s; tn-kind of tn-save-tn: ~s~%"
		  tn (tn-kind tn) sc
		  sb (sb-name sb) (sb-kind sb)
		  offset end
		  original
		  (tn-save-tn tn) (tn-kind (tn-save-tn tn))))

    ;; FIX why implementation dependent here?
    ;;
    (when (backend-featurep :x86)
       ;; On the x86 ports the old-fp and return-pc are often passed
       ;; on the stack so the above hack for the other ports does not
       ;; always work.  Here the old-fp and return-pc tns are
       ;; identified by being on the stack in their standard save
       ;; locations.
       (when (and (not (eq (tn-kind tn) :specified-save))
		  (not (and (string= (sb-name sb) "STACK")
			    (or (= offset 0)
				(= offset 1))))
		  (conflicts-in-sc original sc offset))
	 (error "~S wired to a location that it conflicts with." tn)))

    (add-location-conflicts original sc offset)))

(defevent repack-block "Repacked a block due to TN unpacking.")

;;; Pack  --  Interface
;;;
(defun pack (component)
  (assert (not *in-pack*))
  (let ((*in-pack* t)
	(optimize (policy nil (or (>= speed cspeed) (>= space cspeed))))
	(2comp (component-info component)))
    (init-sb-vectors component)
    ;;
    ;; Call the target functions.
    (do-ir2-blocks (block component)
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
	  ((null vop))
	(let ((target-fun (vop-info-target-function (vop-info vop))))
	  (when target-fun
	    (funcall target-fun vop)))))

    ;;
    ;; Pack wired TNs first.
    (do ((tn (ir2-component-wired-tns 2comp) (tn-next tn)))
	((null tn))
      (pack-wired-tn tn))
    ;;
    ;; Pack restricted component TNs.
    (do ((tn (ir2-component-restricted-tns 2comp) (tn-next tn)))
	((null tn))
      (when (eq (tn-kind tn) :component)
	(pack-tn tn t)))
    ;;
    ;; Pack other restricted TNs.
    (do ((tn (ir2-component-restricted-tns 2comp) (tn-next tn)))
	((null tn))
      (unless (tn-offset tn)
	(pack-tn tn t)))
    ;;
    ;; Assign costs to normal TNs so we know which ones should always be
    ;; packed on the stack.
    (when (and optimize pack-assign-costs)
      (assign-tn-costs component))
    ;;
    ;; Pack normal TNs in the order that they appear in the code.  This
    ;; should have some tendency to pack important TNs first, since control
    ;; analysis favors the drop-through.  This should also help targeting,
    ;; since we will pack the target TN soon after we determine the location
    ;; of the targeting TN.
    (do-ir2-blocks (block component)
      (let ((ltns (ir2-block-local-tns block)))
	(do ((i (1- (ir2-block-local-tn-count block)) (1- i)))
	    ((minusp i))
	  (declare (fixnum i))
	  (let ((tn (svref ltns i)))
	    (unless (or (null tn) (eq tn :more) (tn-offset tn))
	      (pack-tn tn nil))))))
    ;;
    ;; Pack any leftover normal TNs.  This is to deal with :MORE TNs, which
    ;; could possibly not appear in any local TN map.
    (do ((tn (ir2-component-normal-tns 2comp) (tn-next tn)))
	((null tn))
      (unless (tn-offset tn)
	(pack-tn tn nil)))
    ;;
    ;; Do load TN packing and emit saves.
    (let ((*repack-blocks* nil))
      (cond ((and optimize pack-optimize-saves)
	     (optimized-emit-saves component)
	     (do-ir2-blocks (block component)
	       (pack-load-tns block)))
	    (t
	     (do-ir2-blocks (block component)
	       (emit-saves block)
	       (pack-load-tns block))))
      (when *repack-blocks*
	(loop
	  (when (zerop (hash-table-count *repack-blocks*)) (return))
	  (maphash #'(lambda (block v)
		       (declare (ignore v))
		       (remhash block *repack-blocks*)
		       (event repack-block)
		       (pack-load-tns block))
		   *repack-blocks*)))))

  (undefined-value))
