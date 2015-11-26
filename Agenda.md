# Introduction #

The goal of the b2asm project is to develop a model-based approach that will provide assembly level code generation. Using the well-established **B method**, we will be able to proved a 100% guarantee that the generated code is faithful to the initial model. The b2asm project is therefore aiming to develop a _verifying compiler_, and to contribute fulfilling one of the grand challenges of computer science, stated by Tony Hoare.

# Details #

The project is currently in a phase where we try to establish the feasibility of the approach. Our methodology is to:
  * build abstract (B) models of computing platforms, such as the instruction set of a family of micro-controllers;
  * develop small B projects down to assembly level, using the abovementioned of the corresponding instruction sets.

Several such examples have been developped and stored in the source code repository. Consult the SourceRepositoryOrganization for further details.

The next step of the project is to develop rules to automatically generate assembly level implementations from traditional algorithmic level implementations.