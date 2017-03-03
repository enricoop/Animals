(deftemplate node
	(slot name)
	(slot type)
	(slot question)
	(slot yes-node)
	(slot no-node)
	(slot answer)
)
(deftemplate count (slot number))

(deffunction get-count ()
	(progn$ (?f (get-fact-list))
		(if (eq (fact-relation ?f) count)
			then (return ?f)))
	)

(deffunction get-node-by-name (?name)
	(progn$ (?f (get-fact-list))
		(if (and (eq (fact-relation ?f) node) (eq (fact-slot-value ?f name) ?name) ) then (return ?f))
	)
)

(deffunction replace-answer-node (?name)
	(printout t "Quale animale e'? ")
	(bind ?new (read))
	(bind ?node (get-node-by-name ?name))
	(bind ?old (fact-slot-value ?node answer))
	(printout t "A quale domanda la cui risposta sia yes mi fara' distinguere un " ?new " da un " ?old " ? ")
	(bind ?quest (readline))
	
	(bind ?factcount (get-count))
	(bind ?n (fact-slot-value ?factcount number))
	(assert (node (name (sym-cat node ?n)) (type answer) (question nil) (yes-node nil) (no-node nil) (answer ?old)))	
	
	(assert (node (name ?name) (type decision) (question ?quest) (yes-node (sym-cat node (+ ?n 1))) (no-node (sym-cat node ?n)) (answer nil)))
	(bind ?n (+ ?n 1))
	(assert (node (name (sym-cat node ?n)) (type answer) (question nil) (yes-node nil) (no-node nil) (answer ?new)))
	(retract ?factcount)
	(assert (count (number (+ ?n 1))))
	
	(printout t "adesso conosco " ?new crlf)
	(return )
)

(defrule initialize
	(not (node (name root)))
	=>
	(load-facts "ANIMAL.DAT") (assert (current-node root))	
	)
	
;The search advances by propely setting the current-node fact	
(defrule ask-decision-node-question
	?node <- (current-node ?name)
	(node (name ?name) (type decision) (question ?question))
	(not (answer ?))
	=>
	(printout t ?question  "(yes or no) ")
	(assert (answer (read)))
)

(defrule proceed-to-yes-branch
	?node <- (current-node ?name)
	(node (name ?name) (type decision) (yes-node ?yes-branch))
	?answer <- (answer yes)
	=>
	(retract ?node ?answer)
	(assert (current-node ?yes-branch))
)

(defrule proceed-to-no-branch
	?node <- (current-node ?name)
	(node (name ?name) (type decision) (no-node ?no-branch))
	?answer <- (answer no)
	=>
	(retract ?node ?answer)
	(assert (current-node ?no-branch))
)

(deffunction clear-all-fact ($?names)
	(progn$ (?f (get-fact-list))
		(if (not (member$ (fact-relation ?f) $?names))
			then (retract ?f)))
	(return ))

(defrule ask-if-answer-node-is-correct
	?node <- (current-node ?name)
	?curr <- (node (name ?name) (type answer) (answer ?value))
	(not (answer ?))
	=>
	(printout t "Io credo sia un " ?value crlf)
	(printout t "Corretto? (yes or no)" )
	(assert (answer (read)))
)

(defrule answer-node-is-incorrect
	?node <- (current-node ?name)
	?node1 <- (node (name ?name) (type answer))
	?answer <- (answer no)
	(count (number ?n))
	=>
	(replace-answer-node ?name)
	(retract ?node ?answer ?node1)
	;(clear-all-fact node)
	;(assert (count (number ?n)))
	(save-facts "ANIMAL.DAT"  visible count node)
)



