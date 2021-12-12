# Downhill

Reverse mode automatic differentiation in Haskell style.

---

## Overview

*Downhill* library proposes an approach to automatic differentiation
that is both well typed and simple.

Like [Backprop](https://backprop.jle.im/), it allows variables
to have different types.
Traditionally reverse mode automatic differentiation
works by constructing computational graph or Wengert list.
However, faithfully representing heterogeneous
graph in Haskell would result in horrible types.

This library exploits linearity of derivative and constructs *linear*
computational graph to keep everything simple. Bringing heterogeneity
to linear graphs don't make them more complicated. It makes
working with them much easier, thanks to parametric polymorphism.

## Related work

### Backprop

[Backprop](https://backprop.jle.im/) introduced heterogeneous
reverse mode differentiation as a usable library. It has a
well typed and nice interface, but it relies on rather complicated
machinery under the hood. *Downhill* library explores alternative ways
to implement automatic differentiation.

### Conal Elliott's paper

[*The Simple Essence of Automatic Differentiation*](http://conal.net/papers/essence-of-ad/)
by Conal Elliott
explains how gradient is a linear map and all chain rules is just
compositions of linear maps. The idea of linear graphs was inspired
by this paper.
