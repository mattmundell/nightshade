;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/control.lisp,v 1.14 1994/10/31 04:27:28 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    The control analysis pass in the compiler.  This pass determines the
;;; order in which the IR2 blocks are to be emitted, attempting to minimize the
;;; associated branching costs.
;;;
;;;    At this point, we commit to generating IR2 (and ultimately assembler)
;;; for reachable blocks.  Before this phase there might be blocks that are
;;; unreachable but still appear in the DFO, due in inadequate optimization,
;;; etc.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")


;;; Add-To-Emit-Order  --  Interface
;;;
;;;    Insert Block in the emission order after the block After.
;;;
(defun add-to-emit-order (block after)
  (declare (type block-annotation block after))
  (let ((next (block-annotation-next after)))
    (setf (block-annotation-next after) block)
    (setf (block-annotation-prev block) after)
    (setf (block-annotation-next block) next)
    (setf (block-annotation-prev next) block))
  (undefined-value))


;;; FIND-ROTATED-LOOP-HEAD  --  Internal
;;;
;;;    If Block looks like the head of a loop, then attempt to rotate it.  A
;;; block looks like a loop head if the number of some predecessor is less than
;;; the block's number.  Since blocks are numbered in reverse DFN, this will
;;; identify loop heads in a reducible flow graph.
;;;
;;;    When we find a suspected loop head, we scan back from the tail to find
;;; an alternate loop head.  This substitution preserves the correctness of the
;;; walk, since the old head can be reached from the new head.  We determine
;;; the new head by scanning as far back as we can find increasing block
;;; numbers.  Beats me if this is in general optimal, but it works in simple
;;; cases.
;;;
;;; This optimization is inhibited in functions with NLX EPs, since it is hard
;;; to do this without possibly messing up the special-case walking from NLX
;;; EPs described in CONTROL-ANALYZE-1-FUN.  We also suppress rotation of loop
;;; heads which are the start of a function (i.e. tail calls), as the debugger
;;; wants functions to start at the start.
;;;
(defun find-rotated-loop-head (block)
  (declare (type cblock block))
  (let* ((num (block-number block))
	 (env (block-environment block))
	 (pred (dolist (pred (block-pred block) nil)
		 (when (and (not (block-flag pred))
			    (eq (block-environment pred) env)
			    (< (block-number pred) num))
		   (return pred)))))
    (cond
     ((and pred
	   (not (environment-nlx-info env))
	   (not (eq (node-block (lambda-bind (block-home-lambda block)))
		    block)))
      (let ((current pred)
	    (current-num (block-number pred)))
	(block DONE
	  (loop
	    (dolist (pred (block-pred current) (return-from DONE))
	      (when (eq pred block)
		(return-from DONE))
	      (when (and (not (block-flag pred))
			 (eq (block-environment pred) env)
			 (> (block-number pred) current-num))
		(setq current pred   current-num (block-number pred))
		(return)))))
	(assert (not (block-flag current)))
	current))
     (t
      block))))
	  

;;; Control-Analyze-Block  --  Internal
;;;
;;;    Do a graph walk linking blocks into the emit order as we go.  We call
;;; FIND-ROTATED-LOOP-HEAD to do while-loop optimization.
;;;
;;;    We treat blocks ending in tail local calls to other environments
;;; specially.  We can't walked the called function immediately, since it is in
;;; a different function and we must keep the code for a function contiguous.
;;; Instead, we return the function that we want to call so that it can be
;;; walked as soon as possible, which is hopefully immediately.
;;;
;;;    If any of the recursive calls ends in a tail local call, then we return
;;; the last such function, since it is the only one we can possibly drop
;;; through to.  (But it doesn't have to be from the last block walked, since
;;; that call might not have added anything.)
;;;
;;;    We defer walking successors whose successor is the component tail (end
;;; in an error, NLX or tail full call.)  This is to discourage making error
;;; code the drop-through.
;;;
(defun control-analyze-block (block tail block-info-constructor)
  (declare (type cblock block) (type block-annotation tail))
  (unless (block-flag block)
    (let ((block (find-rotated-loop-head block)))
      (setf (block-flag block) t)
      (assert (and (block-component block) (not (block-delete-p block))))
      (add-to-emit-order (or (block-info block)
			     (setf (block-info block)
				   (funcall block-info-constructor block)))
			 (block-annotation-prev tail))
      
      (let ((last (block-last block)))
	(cond ((and (combination-p last) (node-tail-p last)
		    (eq (basic-combination-kind last) :local)
		    (not (eq (node-environment last)
			     (lambda-environment (combination-lambda last)))))
	       (combination-lambda last))
	      (t
	       (let ((component-tail (component-tail (block-component block)))
		     (block-succ (block-succ block))
		     (fun nil))
		 (dolist (succ block-succ)
		   (unless (eq (first (block-succ succ)) component-tail)
		     (let ((res (control-analyze-block
				 succ tail block-info-constructor)))
		       (when res (setq fun res)))))
		 (dolist (succ block-succ)
		   (control-analyze-block succ tail block-info-constructor))
		 fun)))))))


;;; CONTROL-ANALYZE-1-FUN  --  Internal
;;;
;;;    Analyze all of the NLX EPs first to ensure that code reachable only from
;;; a NLX is emitted contiguously with the code reachable from the Bind.  Code
;;; reachable from the Bind is inserted *before* the NLX code so that the Bind
;;; marks the beginning of the code for the function.  If the walks from NLX
;;; EPs reach the bind block, then we just move it to the beginning.
;;;
;;;    If the walk from the bind node encountered a tail local call, then we
;;; start over again there to help the call drop through.  Of course, it will
;;; never get a drop-through if either function has NLX code.
;;;
(defun control-analyze-1-fun (fun component block-info-constructor)
  (declare (type clambda fun) (type component component))
  (let* ((tail-block (block-info (component-tail component)))
	 (prev-block (block-annotation-prev tail-block))
	 (bind-block (node-block (lambda-bind fun))))
    (unless (block-flag bind-block)
      (dolist (nlx (environment-nlx-info (lambda-environment fun)))
	(control-analyze-block (nlx-info-target nlx) tail-block
			       block-info-constructor))
      (cond
       ((block-flag bind-block)
	(let* ((block-note (block-info bind-block))
	       (prev (block-annotation-prev block-note))
	       (next (block-annotation-next block-note)))
	  (setf (block-annotation-prev next) prev)
	  (setf (block-annotation-next prev) next)
	  (add-to-emit-order block-note prev-block)))
       (t
	(let ((new-fun (control-analyze-block bind-block
					      (block-annotation-next
					       prev-block)
					      block-info-constructor)))
	  (when new-fun
	    (control-analyze-1-fun new-fun component
				   block-info-constructor)))))))
  (undefined-value))

  
;;; Control-Analyze  --  Interface
;;;
;;;    Do control analysis on Component, finding the emit order.  Our only
;;; cleverness here is that we walk XEP's first to increase the probability
;;; that the tail call will be a drop-through.
;;;
;;;    When we are done, we delete blocks that weren't reached by the walk.
;;; Some return blocks are made unreachable by LTN without setting
;;; COMPONENT-REANALYZE.  We remove all deleted blocks from the IR2-COMPONENT
;;; VALUES-RECEIVERS to keep stack analysis from getting confused.
;;;
(defevent control-deleted-block "control analysis deleted dead block")
(defun control-analyze (component block-info-constructor)
  (declare (type component component)
	   (type function block-info-constructor))
  (let* ((head (component-head component))
	 (head-block (funcall block-info-constructor head))
	 (tail (component-tail component))
	 (tail-block (funcall block-info-constructor tail)))
    (setf (block-info head) head-block)
    (setf (block-info tail) tail-block)
    (setf (block-annotation-prev tail-block) head-block)
    (setf (block-annotation-next head-block) tail-block)

    (clear-flags component)

    (dolist (fun (component-lambdas component))
      (when (external-entry-point-p fun)
	(control-analyze-1-fun fun component block-info-constructor)))

    (dolist (fun (component-lambdas component))
      (control-analyze-1-fun fun component block-info-constructor))

    (do-blocks (block component)
      (unless (block-flag block)
	(event control-deleted-block (continuation-next (block-start block)))
	(delete-block block))))

  (let ((2comp (component-info component)))
    (when (ir2-component-p 2comp)
      ;; If it's not an ir2-component, don't worry about it.
      (setf (ir2-component-values-receivers 2comp)
	    (delete-if-not #'block-component
			   (ir2-component-values-receivers 2comp)))))

  (undefined-value))
