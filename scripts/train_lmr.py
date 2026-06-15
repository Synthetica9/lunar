from textwrap import indent

import numpy as np
import pandas as pd
from sklearn.metrics import log_loss
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPRegressor

bench_nodes = 1072608055
bench_depth = 20
bench_positions = 50
branch_factor = (bench_nodes / bench_positions) ** (1 / bench_depth)

headers = "depth base_red extra_red nodes reduced_result full_result is_quiet neg_hist neg_see is_check is_cut tt_is_capture ttpv any_moves_pruned lmp_cut is_capture futility_pruning is_se singular_ext is_mate_threat side_to_move_only_kp".split()
df = pd.read_csv(
    "lmr_data_2.csv",
    names=headers,
    # nrows=10000,
)

bool_headers = headers[4:]
feature_headers = bool_headers[2:]
result_headers = bool_headers[:2]

df[bool_headers] = df[bool_headers].map(lambda x: int(x.strip() == "true"))

effective_depth = np.log(df["nodes"]) / np.log(branch_factor)
effective_red = df["depth"] - effective_depth
effective_extra = effective_red - df["base_red"] - 1
diff = effective_extra - df["extra_red"]
diff_norm = (diff - diff.mean()) / diff.std()
diff_norm = diff_norm.clip(-1, 1)

# pd.set_option("display.min_rows", 500)
# pd.set_option("display.max_rows", 500)
# print(
#     pd.DataFrame(
#         {
#             "real_depth": df["depth"],
#             "nodes": df["nodes"],
#             "effective_depth": effective_depth,
#             "effective_red": effective_red,
#             "base_red": df["base_red"],
#             "effective_extra": effective_extra,
#             "extra": df["extra_red"],
#             "diff": diff,
#             "diff_norm": diff_norm,
#         }
#     )
# )

X = df[feature_headers]
learn_rate = 0.01
y = df["extra_red"] + diff_norm * learn_rate

clf = MLPRegressor(
    random_state=1,
    max_iter=500,
    hidden_layer_sizes=(16,),
    # scoring="neg_log_loss",
)

X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=4)

clf.fit(X_train, y_train)

score = clf.score(X_test, y_test)
print("// Score:", score)
# probas = clf.predict_proba(X_test)
# print("// Log loss:", log_loss(y_test, probas))
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
inferred = pd.Series([infer(x) for (_idx, x) in X_test.iloc[:50000].iterrows()])

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


print("use super::{BareAccum, NUM_FEATURES};")
print()
print(f"pub const OFFSET: f64 = {offset};")
print(f"pub const MULT: f64 = {mult};")

print(f"pub const BIAS: BareAccum = {to_accum(bias_1)};")
print("pub const LAYER_1: [BareAccum; NUM_FEATURES] = [")
for xs in layer_1:
    print(indent(to_accum(xs), " " * 4) + ",")
print("];")
print(f"pub const LAYER_2: BareAccum = {to_accum(layer_2.ravel())};")
