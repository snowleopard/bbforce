Generated solution
==================

We have two inverters that we can use. Let’s assume that at first we invert an ‘inverter-free’ function _f_. It turns out that the only choice that works is the [_majority function_](http://en.wikipedia.org/wiki/Majority_function):

> f = ab + ac + bc

Now we can invert another ‘inverter-free’ function _g_ which can depend on four parameters: _a_, _b_, _c_, and _¬f_. Again, we have only one choice for _g_:

> g = ¬f(a + b + c)  + abc

Notice that _g_ is nothing else than an XOR of _a_, _b_, and _c_. Finally, we can try to generate inversion of one of the inputs, say, _a_ using an 'inverter-free' function _h_ which can depend on four parameters: _b_, _c_, _¬f_, _¬g_ (note that _h_ cannot depend on _a_ since it is monotonic). The only function which does the job is:

> h = ¬f(b + c + ¬g) + ¬gbc

It is not difficult to symbolically check that _h = ¬a_ as intended. Since functions _f_ and _g_ are symmetric with respect to _a_, _b_ and _c_, the other two signals _b_ and _c_ can be inverted in the same way.

The solution is quite complex, so it's no wonder I couldn't figure it out by hand.

P.S.: Interesting! I've just noticed that the pair of outputs _(f, g)_ form a [_full adder_](http://en.wikipedia.org/wiki/Adder_\(electronics\)#Full_adder).
