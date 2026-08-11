# Musical improvisation generation algorithms

This project explores various ways to create music-generating algorithms.

The first approach employs the factor oracle, an algorithm originally designed for pattern matching, which offers the advantage of linear time and space complexity.

The second approach is based on Markov chains, generalized to handle transitions of varying lengths. Generation relies on a transition matrix populated during semi-supervised training, which combines human evaluation with automated assessment using edit distance.

Finally, AI-driven chord generation is incorporated using LSTM neural networks.

