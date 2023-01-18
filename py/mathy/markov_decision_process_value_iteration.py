R = {
    # (s, a, s'): R(s, a, s')
    ("cool", "fast", "cool"): 2.0,
    ("cool", "fast", "warm"): 2.0,
    ("cool", "slow", "cool"): 1.0,
    ("warm", "fast", "over"): -10.0,
    ("warm", "slow", "cool"): 1.0,
    ("warm", "slow", "warm"): 1.0,
}

T = {
    # (s, a, s'): T(s, a, s')
    ("cool", "fast", "cool"): 0.5,
    ("cool", "fast", "warm"): 0.5,
    ("cool", "slow", "cool"): 1.0,
    ("warm", "fast", "over"): 1.0,
    ("warm", "slow", "cool"): 0.5,
    ("warm", "slow", "warm"): 0.5,
}

transitions = {
    # s: {a: [s']}
    "cool": {"slow": ["cool"], "fast": ["cool", "warm"]},
    "warm": {"slow": ["cool", "warm"], "fast": ["over"]},
    "over": {},
}


def V(s, k, gamma=1.0):
    if k == 0 or len(transitions[s]) == 0:
        return 0
    return max(
        sum(
            T[(s, a, sp)] * (R[(s, a, sp)] + gamma * V(sp, k - 1, gamma))
            for sp in sps
        )
        for a, sps in transitions[s].items()
    )


for k in range(3):
    row = {s: V(s, k) for s in transitions}
    row = "  ".join(f"{s}={v:0.1f}" for s, v in row.items())
    print(f"V_{k}:  {row}")
