# b2asm
The B method is a rigorous software engineering process that is supported by a notation. The process starts with the creation of an initial model, composed essentially of state and operations. The state is a set of variables and an invariant condition that identifies the safe states. The operations may change the state of the model but shall be such that the invariant is preserved. Proof obligations that guarantee the overall consistency of the model are generated and must be discharged. The initial model is then the subject of successive refinements. Each refinement must also be checked rigorously. The result of the refinement process in the classic B method is an algorithmic level description of the operations that may then be synthesized to source code in a programming language.

The B method is supported by different industrial and academic tools. The purpose of this project is to provide techniques and tool support to extend the refinement process to generate assembly level description of the operations.

* This repository was automatically exported from code.google.com/p/b2asm
