(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag) 
      '(())
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add e to the front of each of these.
      ;; Do this for all possible e to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                            (remove e bag :count 1))))
              bag)))
              
(defun all-edges (g isint)
	"Return all pairs of vertices that have common edge in G"
	(let (edges '())
		(dolist (r g)
			(let ((a (car r)) (blist (cdr r)))
				(dolist (b blist)
					(let ((edge (norm (list a b) isint)))
						(cond
							((member edge edges :test #'equal) nil)
							(t (push edge edges))
						)
					)
				)
			)
		)
		edges
	)
)

(defun norm (edge isint)
	(sort edge (if (eq isint t) #'< #'string-lessp))
)

(defun all-vertices (g)
	"Return all vertices in the graph."
	(mapcar #'car g)
)

(defun find-edge2 (edge1 vertices1 vertices2)
	(let ((edge2 nil))
		(loop for x in vertices1
			for y in vertices2 do
				(cond
					((= (length (member x edge1)) 2)
						(push y edge2) ;; x on the first position in the edge
					)
					((member x edge1)
						(setq edge2 (append edge2 (list y)))
					)
					(t nil)
				)
		)
		edge2
	)
)

(defun iso (g1 g2)
	"Find permutation of all vertices in g2 so that it would mirror g1. g1 vertex labels must be ints"
	(let
		(
			(result nil)
			(edges1 (all-edges g1 t))
			(edges2 (all-edges g2 nil))
			(vertices1 (all-vertices g1))
			(perms2 (permutations (all-vertices g2)))
		)
		(dolist (vertices2 perms2)
			(progn
				(setq found t)
				(dolist (edge1 edges1)
					(progn
						(setq edge2 (norm (find-edge2 edge1 vertices1 vertices2) nil))
						(cond
							((member edge2 edges2 :test #'equal) nil) ;; we have a chance
							(t (setq found nil)) ;; not found one edge, the whole permutation is wrong
						)
					)
				)
				(if (eq found t)
					(setq result vertices2) ;; if found just remember it
				)
			)
		)
		(loop for x in vertices1
			for y in result collect
				(list x y)
		)
	)
)

;; run
;; (iso '((1 3) (2 3) (3 1 2)) '((c b) (a b) (b c a))) // iso
;; (iso '((1 3 2) (2 3 1) (3 1 2)) '((c b) (a b) (b c a))) // non iso
;; (iso '((1 3 4) (2 3) (3 1 2) (4 1)) '((c b) (a b) (b c a d) (d b))) // non iso
;; (iso '((1 3 4 2) (2 3 1) (3 1 2) (4 1)) '((c b d) (a b) (b c a) (d b c))) // iso
