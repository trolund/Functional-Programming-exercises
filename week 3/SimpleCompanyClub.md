# Exercise: Simple Company club

A company has a club for employees, and this club has a register containing names and
descriptions of every club member. You may assume that names are unique. The description
of a member is a tuple (no, yb, ths), where no is a telephone number, yb is the year of birth,
and ths is the themes of interests of the member.
A club arrangement is given by a predicate describing the club members that may be interested in the arrangement. Let, for example, the predicate p1(no, yb, ths) be true for a given
description (no, yb, ths) of a club member when yb is greater than 1982 and ths includes ”soccer” and ”jazz”. This predicate describes an arrangement directed to young club members
that are interested in both soccer and jazz.
Your solution to this exercise should be presented using the model-based approach discussed
in Section 4.6 in the textbook. In particular your solution should contain:

1. Types for the important concepts of the problem formulation including, at least, types
   for the register themes of interests, descriptions and arrangements.
2. A declaration of a register reg, a declaration of an arrangement p1 for the above described arrangement p1, and a declaration of an arrangement p2 that is directed to
   young club members that are interested in either ”soccer” or ”jazz” or both. These
   declarations should be constructed so that they can serve as illustrative examples.
3. A declaration of a function extractTargetGroup p r that gives a list with names and
   phone numbers of the members in register r that may be interested in the arrangement
   p. State the type of extractTargetGroup in a comment. Make use of the type names
   introduced under point 1. above, so that the type reflects the intention with the function.
4. Tests of extractTargetGroup involving reg, p1 and p2.
