## Hokuspokus

Feature transformation for [supervised learning](http://en.wikipedia.org/wiki/Supervised_learning) using equation discovery.

For motivation, background and results see `thesis.pdf`.

## Setup

You need a Common Lisp compiler, like [SBCL](http://www.sbcl.org). To install SBCL with homebrew:

```sh
brew install sbcl
```

Create a link in the default [ASDF](https://common-lisp.net/project/asdf/) systems definition folder to the hokuspokus module:

```sh
ln -s ~/projects/hokuspokus/ ~/common-lisp/
```

## Run

Start SBCL and load the `hokuspokus` module:

```sh
sbcl
```

```lisp
(require :asdf)
(asdf:load-system :hokuspokus)
```

Example run using the `data/kepler` dataset:

```lisp
(hokuspokus:with
   (hokuspokus::in-file #p"data/kepler.arff")
   (hokuspokus::operators '+ '- '* '/ 'sqrt)
   (hokuspokus::pre-select :percent 0.25)
   (hokuspokus::post-select :abs 4)
   (hokuspokus::depth 3))
```

Look at the resulting feature space:

```lisp
hokuspokus::*feature-space*
```

One of the resulting formulas can be simplified to Kepler's third law.

Kepler's Third Law: r^3/T^2

Matching formula:

`(((r * r) * (r * r)) / ((t * r) * t))`

Reduces to:

`(r * r * r) / (t * t)`

## Weka integration

Data files are read and written in the [Weka](http://www.cs.waikato.ac.nz/ml/weka/) [ARFF](https://weka.wikispaces.com/ARFF) format.

To write the results of a run back to an ARFF file:

```lisp
(hokuspokus:with
   (hokuspokus::in-file #p"data/kepler.arff")
   (hokuspokus::operators '+ '- '* '/ 'sqrt)
   (hokuspokus::pre-select :percent 0.25)
   (hokuspokus::post-select :abs 4)
   (hokuspokus::out-file #p"data/kepler.processed.arff")
   (hokuspokus::depth 3))
```

## Limitations

Only numerical, non-sparse data sets are supported.
