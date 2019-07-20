# ProcessingGame

This application presents to the user the multiprocessor scheduling problem in the form of a game. The multiprocessor scheduling problem arises because of limitations in predicting processor needs. In a perfect world, where we can predict the processing needs ahead of time, we could ensure that each processor acts in unison to finish applications. Individual threads stack neatly, distributed among the processors to allocate resources in the most optimium way. However, that scenario isn't the case. Instead, we have to try to minimize waste of processor time. It can come down to a guess or sophisticated strategy. In the end, optimimum resource allocation is unlikely.

This application uses the game concept of an "ado" -- some part of a program that needs the attention of a processor. A "program" contains multiple "ados". The "processor" takes time to work on an ado. A processor can only move onto the next ado after all ados of a program finish. This blocking simulates non-optimum ordering. Ados vary in size. Depending on what ados are already in a processor, the time to process any ado will vary. As a result, the ultimate time to handle a program varies. A program isn't processed until all its ados leave the processors that worked on them. How the ados stack affect action of completion.

In addition to the aim of exploring the multiprocessor scheduling problem in the form of a game, this application also uses functional programming. This paradigm allows for a terser look at the problem. Functions do only simple operations. The overall program carries around state, known as the "environment". 
