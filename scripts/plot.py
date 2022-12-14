#!/usr/bin/env nix-shell
#!nix-shell -p "python3.withPackages (p: with p; [ matplotlib numpy scipy imageio watchdog tqdm ])" -i python

from matplotlib import pyplot as plt
import numpy as np
from scipy.stats import gaussian_kde, linregress
import glob
import imageio
import time
import shutil
from tqdm import tqdm


from watchdog.events import FileSystemEventHandler
from watchdog.observers import Observer


def plot_err(data, ax=None):
    if ax is None:
        ax = plt.axes()

    # last_xlabel = list(ax.axes.get_xticklabels())[-1]
    ax.xaxis.set_ticklabels([])
    # last_xlabel.set_visible(False)
    # last_xlabel.set_fontsize(0.0)

    (y,) = data
    plt.plot(y, "+")


def plot_density(data, ax=None):
    if ax is None:
        ax = plt.axes()
    # https://stackoverflow.com/a/20107592
    try:
        x, y = (data - 0.5) * 2
    except TypeError:
        return
    z = gaussian_kde(data)(data)
    idx = z.argsort()
    x, y, z = x[idx], y[idx], z[idx]

    z = np.log10(z)

    ax.scatter(x, y, c=z, label="Points", marker=".")

    regress = linregress(x, y)
    a, b = regress.slope, regress.intercept
    f = lambda v: a * v + b

    ax.set_ylim(-1, 1)
    ax.set_xlim(-1, 1)
    ax.plot([-1, 1], [-1, 1], label="Target", color="tab:gray")
    ax.plot(
        [-1, 1],
        [f(-1), f(1)],
        label=f"Linear regression ({a=:.3f})",
        color="tab:orange",
    )
    ax.legend()


def load(filename):
    res = []
    with open(filename) as f:
        for line in f:
            res.append(list(float(x) for x in line.split("\t") if x.strip()))

    return np.array(res).T


def render_frame():
    fig, (ax1, ax2) = plt.subplots(1, 2)
    fig.set_figheight(8)
    fig.set_figwidth(16)

    curve = load("curve.dat")
    plot_density(curve, ax1)

    err = load("error.dat")
    (_, i) = err.shape
    print(i)
    plot_err(err, ax2)

    fig.tight_layout()

    fig.savefig(f"frames/frame{i:05}.png")

    plt.close(fig)


def make_animation():
    filenames = glob.glob("frames/frame*.png")

    images = []
    for filename in tqdm(sorted(filenames)):
        images.append(imageio.imread(filename))

    imageio.mimsave("out_partial.gif", images)
    shutil.move("out_partial.gif", "out.gif")


class FrameHandler(FileSystemEventHandler):
    def on_any_event(self, event):
        time.sleep(0.01)
        render_frame()
        make_animation()


def main():
    observer = Observer()
    observer.schedule(FrameHandler(), "error.dat")
    observer.start()
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()


if __name__ == "__main__":
    main()
