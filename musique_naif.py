import numpy as np
import random as rd
import time
from pyo import *
import matplotlib.pyplot as plt
import seaborn as sns

lettre_a_elise = ["mi", "re#", "mi", "re#", "mi", "si", "re", "do", "la", "do", "mi", "la", "si", "mi", "sol#", "si", 
         "do", "mi", "mi", "re#", "mi", "re#", "mi", "si", "re", "do", "la", "do", "mi", "la", "si", "mi", 
         "do", "si", "la", "si", "do", "re", "mi", "sol", "fa", "mi", "re", "fa", "mi", "re", "do", "mi", 
         "re", "do", "si", "mi", "re#", "mi", "re#", "mi", "si", "re", "do", "la", "do", "mi", "la", "si", 
         "mi", "sol#", "si", "do", "mi", "mi", "re#", "mi", "re#", "mi", "si", "re", "do", "la", "do", "mi", 
         "la", "si", "mi", "do", "si", "la"]

ode_a_la_joie=["mi","mi","fa","sol","sol","fa","mi","re","do","do","re","mi","mi","re","re",
               "mi","mi","fa","sol","sol","fa","mi","re","do","do","re","mi","re","do","do",
               "re","re","mi","do","re","mi","fa","mi","do","re","mi","fa","mi","re","do","re","sol",
               "mi","mi","fa","sol","sol","fa","mi","re","do","do","re","mi","re","do","do"]

bach_solfeggietto = ["mib3","do3","mib3","sol3","do4","mib4","re4","do4","si3","sol3","si3","re4","sol4","fa4","mib4","re4","mib4","do4",
                     "mib4","sol4","do5","mib5","re5","do5","re5","do5","si4","la4","sol4","fa4","mib4","re4","mib4","do4","mib4","sol4","do5",
                     "mib5","re5","do5","si4","sol4","si4","re5","sol5","fa5","mib5","re5","mib5","do5","mib5","sol5","do6","mib6","re6","do6",
                     "re6","do6","si5","la5","sol5","fa5","mib5","re5","mib5",
                     "do5","sol4","mib4","do4","do6","sol5","mib5","lab5","fa3","lab3","do4","fa4","lab4","do5","mib5","re5","sib4","fa4","re4","sib3","sib5","fa5","re5","sol5","mib3","sol3","sib3","mib4","sol4","sib4",
                     "re5","do5","la4","lab4","la4","do5","la4","lab4","la4","mib5","do5","sol4","la4","mib5","do5","sol4","la4","re5","do5","solb4","la4","la5","do5","solb4","la4","la5","do5","re4","la4","do5","la4","solb4","re4","sib4"]


def construction_liste_notes(notes_musique):
    """
    Creé la liste des notes utilisées dans la musique
    (uniquement celles qu'on utilisera dans l'impro)
    """
    vues = set()
    uniques = []
    for note in notes_musique:
        if note not in vues:
            uniques.append(note)
            vues.add(note)
    return uniques
    
def init_matrice_transition(lst_notes):
    """
    initialise une matrice carree de taille le nombre de notes utilisées dans la musique.
    """
    n=len(lst_notes)
    mat = []
    for i in range(0,n):
        temp=[0 for j in range(n)]
        mat.append(temp)
    return mat

def numero_associe_note(l,note):
    """
    renvoie l'indice d'une note dans le tableau des notes utilisées.
    """
    for i in range(len(l)):
        if note==l[i]:
            return i
    return -1

def rempli_matrice(musique):
    """
    prerempli la matrice de transition : pour l'instant, on met simplement dans
    la case (i,j) le nombre de fois où l'on passe de la note i à la note j
    """
    l=construction_liste_notes(musique)
    m = init_matrice_transition(l)
    for i in range(len(musique)-1):
        a=numero_associe_note(l,musique[i])
        b=numero_associe_note(l,musique[i+1])
        m[a][b]+=1
    return m

def matrice_proba(musique):
    """
    Modifie la matrice pour que chaque case (i,j) soit proportionnelle à la transition de la note i à j.
    """
    m = rempli_matrice(musique)
    n=len(m)
    for i in range(n):
        total=0
        for j in range(n):
            total+=m[i][j]
        for j in range(n):
            m[i][j]=m[i][j]/total
    return m
        
def genere_melodie_v1(musique):
    """
    Commence par générer aléatoirement la note initiale.
    Puis choisit les notes suivantes à l'aide de la matrice de transition.
    """
    longueur_melodie=len(musique)
    matrice_transition=matrice_proba(musique)
    lst_notes=construction_liste_notes(musique)
    
    note_actuelle = rd.choice(lst_notes)
    melodie = [note_actuelle]
    
    for _ in range(longueur_melodie-1):
        index_actuel = numero_associe_note(lst_notes, note_actuelle)
        probabilites = matrice_transition[index_actuel]
        note_actuelle = rd.choices(lst_notes, weights=probabilites)[0]
        melodie.append(note_actuelle)
    return melodie


def N_grammes(musique,N):
    lst_notes=construction_liste_notes(musique)
    n=len(lst_notes)
    m=len(musique)
    mat = []
    
    for i in range(0,n):
        mat.append([])
        
    lst_transitions = []
    facteur_actuel = []
    nb=0
    
    for i in range(0,N):
        facteur_actuel.append(musique[i])
    
    for j in range(N,m+1):
        if facteur_actuel[1:] not in lst_transitions:
            y = facteur_actuel[1:]
            lst_transitions.append(y)
            for k in range(0,n):
                if k == numero_associe_note(lst_notes,facteur_actuel[0]):
                    mat[k].append(1)
                else:
                    mat[k].append(0)
            nb+=1
        else:
            mat[numero_associe_note(lst_notes,facteur_actuel[0])][lst_transitions.index(facteur_actuel[1:])]+=1
            
        if j<m:
            facteur_actuel.append(musique[j])
            del facteur_actuel[0]
            
    for i in range(n):
        total=0
        for j in range(nb):
            total+=mat[i][j]
        if total != 0 :    
            for j in range(nb):
                mat[i][j]=mat[i][j]/total    
    
    return lst_transitions,mat
    

def concatenation_proba(mat1,mat2,lst1,lst2):
    """
    Concatene deux matrices (mêmes indices de lignes)
    """
    t1 = np.array(mat1) / 2
    t2 = np.array(mat2) / 2
    lst = lst1 + lst2
    mat_concat = np.concatenate((t1, t2), axis=1)
    return lst, mat_concat

def matrice_tout_facteurs_borne(musique,borne):
    """
    Applique les N grammes jusqu'à une borne désirée
    """
    (lst, mat1) =construction_liste_notes(musique), matrice_proba(musique)
    lst1 = []
    for elt in lst :
        lst1.append([elt])
    
    for i in range(3,borne+1):
        (lst2,mat2)=N_grammes(musique,i)
        (lst1,mat1)=concatenation_proba(mat1,mat2,lst1,lst2)
    
    return lst1,mat1




def generer(musique,matrice,liste_transi,taille):
    """
    En utilisant la matrice de probabilités, on choisit aléatoirement des transitions afin de construire une musique.
    """
    lst_notes=construction_liste_notes(musique)
    note_actuelle = rd.choice(lst_notes)
    melodie = np.array([note_actuelle])
    
    transi_utilisees = []
    
    while len(melodie)<taille:
        note_actuelle = melodie[-1]
        index_actuel = numero_associe_note(lst_notes, note_actuelle)
        probabilites = matrice[index_actuel]
        sequence_actuelle = rd.choices(liste_transi, weights=probabilites)[0]
        melodie = np.concatenate((melodie,np.array(sequence_actuelle)))
        transi_utilisees.append((index_actuel,liste_transi.index(sequence_actuelle) ))
    
    return melodie[:taille],transi_utilisees




def distance(arr1, arr2):
    """
    Renvoie la distance de Levenshtein entre deux musiques
    
    """
    arr1 = np.array(arr1, dtype=object)
    arr2 = np.array(arr2, dtype=object)
    len1, len2 = len(arr1), len(arr2)
    dp = np.zeros((len1 + 1, len2 + 1), dtype=int)

    for i in range(len1 + 1):
        dp[i][0] = i
    for j in range(len2 + 1):
        dp[0][j] = j

    for i in range(1, len1 + 1):
        for j in range(1, len2 + 1):
            if np.array_equal(arr1[i - 1], arr2[j - 1]):
                cost = 0
            else:
                cost = 1
            dp[i][j] = min(
                dp[i - 1][j] + 1,
                dp[i][j - 1] + 1,
                dp[i - 1][j - 1] + cost
            )

    return dp[len1][len2]




def proportion_efficacite(musique_originale, musique_test, taille):
    notes_possibles = construction_liste_notes(musique_originale)
    distances = []
    for _ in range(1000):
        musique_random = [rd.choice(notes_possibles) for _ in range(taille)]
        d = distance(musique_originale, musique_random)
        distances.append(d)

    distance_moyenne = np.mean(distances)
    distance_test = distance(musique_originale, musique_test)

    if distance_test >= distance_moyenne:
        return 0
    else:
        proportion = 1 - abs(distance_test - (distance_moyenne / 2)) / (distance_moyenne / 2)
        return proportion



def note_to_midi(note_octave):
    """
    Convertit une note avec octave (ex: "do3", "fa#5", "sib2") en valeur MIDI.
    """
    notes_midi = {
        "do": 0, "do#": 1, "reb": 1, "re": 2, "re#": 3, "mib": 3, "mi": 4,
        "fa": 5, "fa#": 6, "solb": 6, "sol": 7, "sol#": 8, "lab": 8,
        "la": 9, "la#": 10, "sib": 10, "si": 11
    }

    # Séparer la partie "note" et "chiffre" dans la chaîne
    import re
    match = re.match(r"([a-z#b]+)(-?\d+)", note_octave)
    if not match:
        raise ValueError(f"Format invalide pour la note : {note_octave}")
    
    note, octave = match.group(1), int(match.group(2))
    if note not in notes_midi:
        raise ValueError(f"Note inconnue : {note}")
    
    return 12 * (octave + 1) + notes_midi[note]

def jouer_musique(musique, bpm=120, duree_note=0.14):
    s = Server().boot()
    s.start()
    
    freqs = [midiToHz(note_to_midi(note)) for note in musique]

    for f in freqs:
        sine = Sine(freq=f, mul=0.2).out()
        time.sleep(duree_note)
        sine.stop()

    s.stop()



def evaluation_complete(musique_originale, musique_test, taille):
    jouer_musique(musique_test)

    note_utilisateur = input("Attribuez une note à la musique (0 à 5) : ")
    try:
        note_utilisateur = int(note_utilisateur)
    except ValueError:
        note_utilisateur = 0

    prop = proportion_efficacite(musique_originale, musique_test, taille)

    return 0.8 * (note_utilisateur/5) + 0.2 * prop


def ajuster_matrice_transition_liste(matrice, transitions_utilisees, score_total):
    """
    Ajuste une matrice (liste de listes) de transitions selon le score fourni.

    - matrice : liste de listes (notes x transitions), avec des float/int
    - transitions_utilisees : liste de tuples (ligne, colonne)
    - score_total : entre 0 et 1 (score final pondéré humain / ressemblance)
    - alpha : intensité d'ajustement
    """
    alpha = 0.7
    n = len(matrice)

    for x, y in transitions_utilisees:
        # Protection contre indices hors limites
        if x < 0 or x >= len(matrice) or y < 0 or y >= len(matrice[x]):
            continue

        influence = (score_total - 0.5) * 2  # -1 à +1
        ajustement = alpha * influence * matrice[x][y]
        matrice[x][y] += ajustement
        matrice[x][y] = max(matrice[x][y], 0.001)  # éviter zéro

    # Renormalisation ligne par ligne
    for i in range(n):
        ligne = matrice[i]
        somme = sum(ligne)
        if somme > 0:
            matrice[i] = [val / somme for val in ligne]

    return matrice


def afficher_matrice_transition(matrice, lst_transitions, vmin=0, vmax=1):
    """
    Affiche une heatmap avec échelle de couleur fixe.
    
    - matrice : liste de listes ou np.array de probabilités
    - lst_transitions : liste des séquences possibles (colonnes)
    - lst_notes : liste des notes (lignes)
    - vmin / vmax : bornes de l'échelle de couleurs (0 à 1 par défaut)
    """
    plt.figure(figsize=(max(10, len(lst_transitions) * 0.5), max(5, len(matrice) * 0.5)))
    ax = sns.heatmap(
        matrice,
        xticklabels=['-'.join(t) for t in lst_transitions],
        yticklabels=lst_transitions[:len(matrice)],
        cmap="YlGnBu",
        vmin=vmin,
        vmax=vmax,
        cbar_kws={'label': 'Probabilité'}
    )
    plt.xticks(rotation=90)
    plt.yticks(rotation=0)
    plt.xlabel("Transitions")
    plt.ylabel("Notes")
    plt.title("Matrice de transitions (probabilités)")
    plt.tight_layout()
    plt.show()


    
def apprentissage(musique, taille, nombre):
    transitions_possibles, mat = matrice_tout_facteurs_borne(musique, 6)
    
    for _ in range(nombre):
        #afficher_matrice_transition(mat, transitions_possibles)
        musique_generee, transi_utilisees = generer(musique, mat, transitions_possibles, taille)
        print(musique_generee)
        score = evaluation_complete(musique, musique_generee, taille)
        mat = ajuster_matrice_transition_liste(mat, transi_utilisees, score)
        
    #afficher_matrice_transition(mat, transitions_possibles) 
        


apprentissage(bach_solfeggietto, 30, 3)

