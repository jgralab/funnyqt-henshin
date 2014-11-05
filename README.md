# funnyqt-henshin

Translate transformations defined with the
[Henshin](https://www.eclipse.org/henshin/)-Editor to equivalent
transformations defined with [FunnyQT](https://github.com/jgralab/funnyqt).

## Usage

When defining a transformation `my-transform` with Henshin, you usually have
the following directory structure:

````
my-transform/
  |-> my-transform.henshin          # The transformation model
  |-> my-transform.henshin_diagram  # The transformation diagram visualizing the model
  |-> my-model.xmi                  # A model the transformation operates on
  `-> my-metamodel.ecore            # The metamodel my-model.xmi conforms to
````

In Henshin terminology, the directory `my-transform` is called the base-path of
the Henshin resource set containing all those files.

To translate the Henshin transformation model `my-transforms.henshin` into
an equivalent FunnyQT transformation, execute the following:

```` clojure
(henshin-to-funnyqt "path/to/my-transform/"  ;; base-path
	                "my-transform.henshin"   ;; transformation model
                    org.myself.my-transform  ;; Namespace into which to generate
                    mt)                      ;; Alias under which to require
					                         ;; org.myself.my-transform
````

After doing that, the target namespace `org.myself.my-transform` contains one
function per top-level Henshin unit defined in the transformation model.  All
those functions have the same name as their Henshin counterparts, and they also
take the same parameters except that there is an additional first parameter
denoting the model on which to execute the rule/unit.

So assuming `my-transform.henshin` defines a rule
`createAccount(client:EString,accoundId:EInt)` (see the
[Henshin Bank example](https://www.eclipse.org/henshin/examples.php?example=bank))
the corresponding generated FunnyQT/Clojure function has the same name and the
parameter vector `[model client accountId]`.  Thus you can apply it using
`(mt/createAccount my-bank-model "John Doe" 1626891)`.


## Supported features

At the current point in time, the following Henshin units can be translated:

- Top-level Henshin *Rules* are translated to FunnyQT in-place transformation
  rules (see
  [defrule](http://userpages.uni-koblenz.de/~horn/funnyqt-docs/funnyqt.in-place.html#IDdefrule)).

  - Simple rules, i.e., matching some elements, preserving some and deleting
    some others, and creating new ones, are supported.
  - Attribute conditions and Attribute Modifications are supported with some
    restrictions.  Henshin uses a JavaScript engine to perform computations on
    attribute values.  Clearly, there is no general JavaScript-to-Clojure
    translation.  However, plain arithmetical expressions and string
    concatenations are translated automatically.  Alternatively, one may enter
    Clojure expressions in the Henshin editor to specify computations on
    attributes.
  - Negative Application Conditions are supported.
  - Positive Application Conditions are supported.
  - Nested Rules (those with *-ed elements) are supported.

- *Sequential Units* are translated to plain Clojure functions.

- *Iterated Units* are translated to plain Clojure functions.


## License

Copyright © 2014 Tassilo Horn <horn@uni-koblenz.de> & The JGraLab Team <ist@uni-koblenz.de>

Distributed under the
[General Public License, Version 3](http://www.gnu.org/copyleft/gpl.html) with
the following additional grant:

    Additional permission under GNU GPL version 3 section 7

    If you modify this Program, or any covered work, by linking or combining it
    with Eclipse (or a modified version of that program or an Eclipse plugin),
    containing parts covered by the terms of the Eclipse Public License (EPL),
    the licensors of this Program grant you additional permission to convey the
    resulting work.  Corresponding Source for a non-source form of such a
    combination shall include the source code for the parts of FunnyQT and
    JGraLab used as well as that of the covered work.
