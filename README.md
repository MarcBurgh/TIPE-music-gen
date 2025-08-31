# Algorithmes générateurs d'improvisation musicale
Ce projet explore différentes manières de créer des algorithmes générant des musiques.

Le premier utilise l'oracle des facteurs, algorithmes initialement conçu pour la recherche de motif, et présente l'avantage de fonctionner linéairement en temps et espace.

Le second se base sur les chaines de Markov, généralisé à des transitions de taille différente. La génération se base sur une matrice de transitions remplie lors de l'entrainement semi-supervisé, où l'on mélange évaluation humaine et automatique avec la distance d'édition.

Enfin, on ajoute une génération d'accords par IA en utilisant les réseaux de neurones LSTM.

