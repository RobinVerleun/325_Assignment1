#| Question 1.

Parameters:
	L1: A list, containing potentially atoms, lists, or NIL. Can be NIL.
	 y: An atom or list or NIL.

Function xmember returns T is argument y is a member of the argument list L1,
and return NIL otherwise. This will also test for lists being members of lists.

Test cases:
(xmember 5 5) => NIL
(xmember nil 5) => NIL
(xmember '(1 2 3 4 5) 5) => T
(xmember '(1 2 (3 4) 5) '(3 4)) => T
(xmember '(1 2 3 4 5) '(3 4)) => NIL

|#
	 
(defun xmember (L1 y)
	(cond
		((atom L1) 			 nil)
		((eq nil L1) 		 nil)
		((equal (car L1) y)    T)
		(t (xmember (cdr L1) y) )
	)	
)

#| Question 2.

Parameters:
	L1: A list, potentially containing lists, atoms, or nils.

A function which takes a list with sublists nested to any depth, and provides a 
result such that the result of (flatten x) is a list of atoms where all atoms
appearing in L1 also appear, in the same order as they appear in L1. 

Flatten serves as a parameter aggregation for the helper function, flatten_helper.

test cases:
(flatten '(a b c d)) => (a b c d)
(flatten '(nil)) => (nil)
(flatten '(a (b c) d)) => (a b c d)
(flatten '(((((a))))) => (a)
(flatten '((a) (b c d) (((e)))) => (a b c d e)
(flatten '(a b c nil)) => (a b c nil)
|#

(defun flatten (L1)
	(flatten_helper L1 nil)
)

#|
A helper function which does what described above for function flatten.
Flatten serves as a parameter aggregation for flatten_helper. L2 is always
intended to be a nil element.

test cases:
(flatten_helper '(a b c d) nil) => (a b c d)
(flatten_helper '(nil) nil) => (nil)
(flatten_helper '(a (b c) d) nil) => (a b c d)
(flatten_helper '(((((a)))) nil) => (a)
(flatten_helper '((a) (b c d) (((e))) nil) => (a b c d e)
(flatten_helper '(a b c nil) nil) => (a b c nil)
|#

(defun flatten_helper (L1 L2)

	(cond
		((equal L1 nil)
			L2)
		((atom (car L1))
			(cons (car L1) (flatten_helper (cdr L1) L2)))	
		(t
			(flatten_helper (car L1) (flatten_helper (cdr L1) L2)))
	)
)

#| Question 3

Parameters:
	L1: A list, containing atoms, sublists, or nils.
	L2: A list, contianing atoms, sublists, or nils.

The elements of L1 and L2 are mixed into a single list, by choosing elements
from L1 and L2 alternatingly. If one list is shorter than the other, then 
append all elements from the longer list at the end.

test cases:
(mix '(1 2 3) '(4 5 6)) => (4 1 5 2 6 3)
(mix '(1 2) '(4 5 6 7)) => (4 1 5 2 6 7)
(mix '(1 (2 3)) '(4 5 6)) => (4 1 5 (2 3) 6)
(mix '(nil) '(4 5 6)) => (4 nil 5 6)
|#

(defun mix (L1 L2)
	(cond
		((null L2) L1)
		((null L1) L2)
		(t
			(cons (car L2) (mix (cdr L2) L1)))
	)
)

#| Question 4

Parameters:
	L: A list to be split into two sublists.

Splits the elements of L into two lists (L1 L2), by putting elements of L
into L1 and L2 alternatingly.

Split acts as a parameter accumulation for split_aggregate.

test cases:
(split '(1 4 2 5 3 6)) => ((1 2 3) (4 5 6))
(split '()) => (nil nil)
(split '(1 (3 4) 2)) => ((1 2) ((3 4)))
|#

(defun split(L)
	(split_aggregate nil nil L)
)


#|
Split aggregate acts as the backbone for the split function. It contains all
the logic, using two empty lists (denoted as E1 and E2) for the function to 
divide list L into.
|#

(defun split_aggregate (E1 E2 L)
	(cond
		((null L) 
			(list E1 E2))
		((null (cdr L))
			(split_aggregate (append E1 (list (car L))) E2 (cddr L)))
		(t
			(split_aggregate (append E1 (list(car L))) (append E2 (list (cadr L))) (cddr L)))
	)
)

;Question 5
#| Part 1
It is untrue that the function combination (split (mix L2 L1)) will consistently
return the list set (L1 L2). There are five base cases to consider:
	1) That the size of L1 is equal to the size of L2.
	2) That L1 contains 1 element more than L2.
	3) That L1 contains 2 or more elements than L2.
	3) That L2 contains 1 more element than L1.
	5) That L2 contains 2 or more elements than L1.
The base cases were determined by both analysis and experimental work. As both 
(split) and (mix) operate on a single atom of each list at a time, the special edge
cases for each of them come from having a single element more than the other list.
Any differend in the element count above two has the same effect as if the difference
was only two.

Below is an analysis of the cases, with examples.

1) The size of L1 is equal to the size of L2.
	
	Example set: L1 = (1 2 3), L2 = (4 5 6)
	(split (mix '(4 5 6) '(1 2 3))) => ((1 2 3) (4 5 6))

	Both split and mix operate in reverse principle when there is equal amounts in
	each list. They undo eachothers behavior with no oddities.

2) L1 contains 1 element more than L2.

	Example set: L1 = (1 2 3), L2 = (4 5)
	(split (mix '(4 5) '(1 2 3))) => ((1 2 3) (4 5))

	In this case, mixing the list results in a special edge case - when the 
	list in the first parameter runs out of the elements, the remainder of the 
	second list is appended to the result. In the case of 1 element more, this
	casues intended behavior, which split undoes.

3) L1 contains 2 or more elements than L2.

	Example set: L1 = (1 2 3), L2 = (4)
	(split (mix '(4) '(1 2 3))) => ((1 2) (4 3))

	Mix begins by making a single list of the elements - (1 4 2 3). This is
	mix working to fix the mismatched sizes. Split, however, evenly disects the
	lists, creating the two sublist (1 2) and (4 3).

4) L2 contains 1 element more than L1.

	Example set: L1 = (1 2), L2 = (3 4 5)
	(split (mix '(3 4 5) '(1 2))) => ((1 2 5) (3 4))

	Mix begins by making the single list (1 3 2 4 5). Split then divides the list
	into ((1 2 5) (3 4)), which are not the original lists.

5) Same issues as 4.

Issue 3, 4, and 5 show how the initial premise is not true.


Part 2
This event will always be true. Split will work to divide the list into two
equal sublists, with the only exception being that the first list could potentially
be longer than the second by exactly one element. Running of the cases described in
Part 1, mixing lists together will only result in a list different from the original
if the second half of the split (cadr (split L)) is two elements smaller than the
first part of the split (car (split L)) (See case 3) or the first part of the split
has one element more than the second (see case 4). Since the split operation will
always, at most, provide one more element to the first half of the split, the mix
operation will always return the original list L. 

|#

#| Question 6

Parameters:
	S: A numeric sum. A positive number.
	L: A list of non-negative integers. Can be empty.

Subsetsum is a function which attempts to find a subset of list L which
adds up to exactly the number S. All numbers in the set L can be used at most
once, and must be represented in the subset in the same order as they appear in 
the list. If no solution can be found, nil is returned. Note: multiple solutions
may exist - the function will return the first one found.

Subsetsum acts as a parameter aggregation for the helper function 'examine_subsets'.

test cases:
(subsetsum 10 '(1 2 3 4 5 6)) => (4 6)
(subsetsum 100 '(10 20 30 40 50 60 70 80 90 100)) => (100)
(subsetsum 7 '(8 9 10)) => (nil)
|#

(defun subsetsum (S L)
	(examine_subsets L S nil)
)

#|
examine_subsets is a helper function for subsetsum.

Parameters:
	L: A list of positive integers.
	sum: a number to add up to.
	E: an empty list, initialized to nil. 

The function acts by subtracting values of the list from the sum value. Because
of this, it is checked to see is sum is below zero, or if sum is equal to zero. 
Both of these cases signal the end of the recursion. If the list of values left
to be checked is nil, the solution has failed, and the recursions returns nil.
Otherwise, the function is recursively called on the two remaining cases - 
the next element of L without modifying sum, or the next element of L while
modifying sum.

This function will eventually check all possible combinations of values to create
a solution if it does not find a solution earlier during the runtime.
|#

(defun examine_subsets (L sum E)
	(cond
		((< sum 0) nil)									 ;if the sum is less than 0, remove this subset
		((= sum 0) E)	 							 ;if the sum is 0, then the subset can be returned
		((null L) nil) 									 ;if the list of numbers is nil, no more work
		((> (car L) sum) (examine_subsets (cdr L) sum E)) ;Remove element which is too large
		(t (or (examine_subsets (cdr L) sum E)
			(examine_subsets (cdr L) (- sum (car L)) (append E (list(car L))))))
	)
)