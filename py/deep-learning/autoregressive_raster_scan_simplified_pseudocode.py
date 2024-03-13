def forward(y):
    y_hat = _quantize(y, "noise")
    means = f(y_hat)
    outputs = _quantize(y, "noise", means)  # Doesn't use means
    outputs = outputs - means
    likelihood = cdf(outputs + 1 / 2) - cdf(outputs - 1 / 2)  # Or something like that


def forward_eval(y):
    y_hat = _quantize(y, "dequantize")  # round
    means = f(y_hat)
    outputs = _quantize(y, "dequantize", means)  # - means, round, + means
    outputs = outputs - means
    # i.e., outputs = (y - means).round()
    likelihood = cdf(outputs + 1 / 2) - cdf(outputs - 1 / 2)


def compress(y):
    means = f(y_hat_prev)
    y_q = _quantize(y_crop, "symbols", means)  # - means, round, int
    y_hat_curr = y_q + means
    encode(y_q)
