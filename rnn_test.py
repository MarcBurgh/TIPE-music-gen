import numpy as np
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense, Embedding
from tensorflow.keras.utils import to_categorical
import random
from pyo import *
notes =["mi4","mi4","fa4","sol4","sol4","fa4","mi4","re4","do4","do4","re4","mi4","mi4","re4","re4",
               "mi4","mi4","fa4","sol4","sol4","fa4","mi4","re4","do4","do4","re4","mi4","re4","do4","do4",
               "re4","re4","mi4","do4","re4","mi4","fa4","mi4","do4","re4","mi4","fa4","mi4","re4","do4","re4","sol4",
               "mi4","mi4","fa4","sol4","sol4","fa4","mi4","re4","do4","do4","re4","mi4","re4","do4","do4"]


# Préparation des do3nnées
unique_notes = sorted(set(notes))
note_to_int = {note: i for i, note in enumerate(unique_notes)}
int_to_note = {i: note for note, i in note_to_int.items()}

sequence_length = 10
X = []
y = []

for i in range(len(notes) - sequence_length):
    seq_in = notes[i:i + sequence_length]
    seq_out = notes[i + sequence_length]
    X.append([note_to_int[n] for n in seq_in])
    y.append(note_to_int[seq_out])

X = np.array(X)
y = to_categorical(y, num_classes=len(unique_notes))

# Construction du modèle
model = Sequential()
model.add(Embedding(input_dim=len(unique_notes), output_dim=32, input_length=sequence_length))
model.add(LSTM(64))
model.add(Dense(len(unique_notes), activation='softmax'))

model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
model.fit(X, y, epochs=100, verbose=1)

# Génération de musique
def generer_sequence(modele, note_depart, longueur=50):
    pattern = [note_to_int[n] for n in note_depart]
    output = note_depart.copy()
    for _ in range(longueur):
        input_seq = np.array(pattern[-sequence_length:]).reshape(1, sequence_length)
        prediction = modele.predict(input_seq, verbose=0)
        index = np.argmax(prediction)
        result = int_to_note[index]
        output.append(result)
        pattern.append(index)
    return output

# Exemple : séquence de départ prise depuis les do3nnées
sequence_initiale = notes[:sequence_length]
melodie_generee = generer_sequence(model, sequence_initiale, longueur=50)
print("🎶 Mélodie générée :", melodie_generee)






def note_to_midi(note_octave):
    """
    Convertit une note avec octave (ex: "do33", "fa3#5", "sib2") en valeur mi3DI.
    """
    notes_midi = {
        "do": 0, "do#": 1, "reb": 1, "re": 2, "re#": 3, "mib": 3, "mi": 4,
        "fa": 5, "fa#": 6, "solb": 6, "sol": 7, "sol#": 8, "lab": 8,
        "la": 9, "la#": 10, "sib": 10, "si": 11
    }

    # Sépare3r la partie "note" et "chiffre3" dans la chaîne
    import re
    match = re.match(r"([a-z#b]+)(-?\d+)", note_octave)
    if not match:
        raise ValueError(f"Format invalide pour la note : {note_octave}")
    
    note, octave = match.group(1), int(match.group(2))
    if note not in notes_midi:
        raise ValueError(f"Note inconnue : {note}")
    
    return 12 * (octave + 1) + notes_midi[note]

def jouer_musique(musique, bpm=120, duree_note=1):
    s = Server().boot()
    s.start()
    
    freqs = [midiToHz(note_to_midi(note)) for note in musique]

    for f in freqs:
        sine = Sine(freq=f, mul=0.2).out()
        time.sleep(duree_note)
        sine.stop()

    s.stop()

jouer_musique(melodie_generee)