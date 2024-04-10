# Automata

A tool for generating synthetic data that results from grammars writen for various automata (eg Finite State Machines, Pushdown Automata and Turing Machines). Grammars are saved as `json`, and you can build 1000 valid strings that match this grammar with:

```sh
automata -i my_grammar.json` -n 1000
```

## PDA example

For instance, there's a grammar that looks like `a^nXb^n`, which is any string like `aaXbb` and `aaaaaXbbbbb`, where the same number of `b`s follow the same number of `a`s (and in the middle is `X`). You cannot write a regex for this (try defining that `n`), but you can write a pushdown automata that recognizes it. A transition function involves the following mappings between states (slightly simplified, see [`rules/anbn.json`](rules/anbn.json)):

```json
{
    "machine": "pda",
    "symbols": ["a", "b", "X"],
    "rules": [
        # input symbol
          |  # current state          # resulting state
          |    |   # top of stack       |    # stack operation
          |    |     |                  |      |
        ["a", "Q1", "A",               "Q1", ["push", "A"]],
        ["X", "Q1", "A",               "Q2", "nullop"],
        ["b", "Q2", "A",               "Q2", "pop"],
    ]
}
```

Notice there are exactly 5 items in each transition rule for a PDA, and they come as 3 on the Left and 2 on the Right. These represent the left and right hand side of the relation that defines a PDA. In english, the rules say:

1. given an input symbol 'a', if I'm in the state Q1, and the top of the stack is "A", let's transition by staying in state Q1, and push an "A" on the stack.

2. if the input symbol is 'X', we know we need to transition to 'Q2', which implies we're gonna start looking for 'b's.

3. each time we see a 'b', pop the stack. This is how we can ensure that the same number of pushes and pops happen, ie, same number of 'b's and 'a's.

Again, see [`rules/anbn.json`](rules/anbn.json)) for the full implementation.


## About the tool


Haskell was chosen because it produces trustworthy code, especially in this department of computer languages. When I next face a funky loss while training some NN, I want to minimize the risk that the data was improperly generated.

A simple `typeclass` is offered to allow the easy (if you speak haskell) specification of new Automata:

```haskell
class Machine m a (s :: Type) where
  data L m a s -- ^ the Left side of a delta function/relation
  data R m a s -- ^ the Right side of a delta function/relation
  data S m a s -- ^ the State of the Machine
  -- | update the state (ex apply stack ops)
  action :: R m a s -> S m a s -> S m a s
  -- | build an input (ex add a peek at the top of a stack)
  mkL :: a -> S m a s -> L m a s
```

There are probably infinite things that are Turing Complete formulations of Turing Machines, and I hope to represent some in this library:

- The classic tape model
- An FSM with 1 queue
- An FSM with 2 stacks
- An FSM with multiple queues
- An FSM with multiple tapes
- Something (i'm not sure what) with addressable memory


## "I just got here and have no clue what you're doing"

That's because I'm bad at explaining things.

I have a project called [`neurallambda`](https://github.com/neurallambda/neurallambda) where I hope to build architectures that augment traditional architectures with reasoning ability. Checkout the readme if you're skeptical, it explains why I don't think the current batch of AI has reasoning.

So for experimenting on these architectures I need data. [Current datasets](https://github.com/neurallambda/awesome-reasoning) are largely around Natural Language, and probably GPT generated (therefore their reasoning is circumspect). It'll be easier to train on super simple perfectly trustworthy datasets, therefore why not generate them ourselves? That's where this lib comes in.
