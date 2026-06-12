from textwrap import indent

import numpy as np
import pandas as pd
from sklearn.metrics import log_loss
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier

headers = "depth base_red extra_red reduced_result full_result is_quiet neg_hist neg_see is_check is_cut tt_is_capture ttpv any_moves_pruned lmp_cut is_capture futility_pruning is_se singular_ext is_mate_threat side_to_move_only_kp".split()
df = pd.read_csv("lmr_data.csv", names=headers)

bool_headers = headers[3:]
feature_headers = bool_headers[2:]
result_headers = bool_headers[:2]

df[bool_headers] = df[bool_headers].map(lambda x: int(x.strip() == "true"))


X = df[feature_headers]
y = (df[result_headers] == [0, 0]).all(axis=1)  # True positives

clf = MLPClassifier(
    random_state=1,
    max_iter=500,
    hidden_layer_sizes=(16,),
    # scoring="neg_log_loss",
)

X_train, X_test, y_train, y_test = train_test_split(X, y, stratify=y, random_state=4)

clf.fit(X_train, y_train)

probas = clf.predict_proba(X_test)
print("// Log loss:", log_loss(y_test, probas))
layer_1, layer_2 = clf.coefs_
bias_1, bias_2 = clf.intercepts_

all_values = np.concat([arr.ravel() for arr in [layer_1, layer_2, bias_1, bias_2]])
max_val = np.abs(all_values).max()
q = 128


def quant(xs):
    return (xs / max_val * q).round().astype(int)


relu = np.vectorize(lambda x: max(x, 0))


def infer(x):
    actv = x
    actv = relu(actv @ quant(layer_1) + quant(bias_1))

    # Don't use relu or bias on 2nd layer because we clip after renormalization
    (actv,) = actv @ quant(layer_2)

    return actv


extra_red = df["extra_red"]
inferred = pd.Series([infer(x) for (_idx, x) in X.iterrows()])

mult = extra_red.std() / inferred.std()
offset = extra_red.mean() - inferred.mean() / inferred.std() * extra_red.std()
rescaled = inferred * mult + offset


def to_accum(xs):
    assert len(xs) == 16

    lines = [
        "BareAccum::from_array([",
        "    " + " ".join(f"{int(x)}," for x in quant(xs)),
        "])",
    ]
    return "\n".join(lines)


print("use super::{BareAccum, NUM_FEATURES}")
print()
print(f"pub const OFFSET: f64 = {offset};")
print(f"pub const MULT: f64 = {mult};")

print(f"pub const BIAS: BareAccum = {to_accum(bias_1)};")
print("pub const LAYER_1: [BareAccum; NUM_FEATURES] = [")
for xs in layer_1:
    print(indent(to_accum(xs), " " * 4) + ",")
print("];")
print(f"pub const LAYER_2: BareAccum = {to_accum(layer_2.ravel())};")
