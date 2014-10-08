gtk2hs-largeTreeStore
=====================

A tree store implementation for gtk2hs that uses nested sets position based iterators,
thus reducing the size needed to store it.

This solves the limitation of the original TreeStore which encodes the full path in the
available iterator bits so has limited depth of the tree depending on the average width
e.g. if average/max at each level width is 128 children per node, then it uses 7 bits for
each level. As it only has storage of 3 Words, it can exceed the encoding space.

LargeTreeStore uses constant size iterators of left and right position elements.

That said, the performance/complexity of each operation is yet to be reviewed.
