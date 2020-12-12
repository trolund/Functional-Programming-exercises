type Tree<'a, 'b> =
    | A of 'a
    | B of 'b
    | Node of Tree<'a, 'b> * Tree<'a, 'b>
