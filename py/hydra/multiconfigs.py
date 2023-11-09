import argparse
import os
import sys

from hydra import compose, initialize
from omegaconf import OmegaConf

DEFAULT_CONFIG_PATH = "conf"
DEFAULT_CONFIG_NAME = "config"
SPECIAL_OPTIONS = ["--config-path", "--config-name"]


def split_argv_groups(argv, special_options):
    argv_groups = [[]]
    visited_options = set()
    for arg in argv:
        for option in special_options:
            if arg.startswith(option):
                if option in visited_options:
                    visited_options = set()
                    argv_groups.append([])
                visited_options.add(option)
        argv_groups[-1].append(arg)
    return argv_groups


def parse_argv_group(argv):
    # A simpler but less "correct" implementation:
    #
    # parser = argparse.ArgumentParser()
    # parser.add_argument("--config-path", type=str, default=DEFAULT_CONFIG_PATH)
    # parser.add_argument("--config-name", type=str, default=DEFAULT_CONFIG_NAME)
    # parser.add_argument("overrides", nargs="*")
    # args = parser.parse_args(argv)

    args = argparse.Namespace()
    args.config_path = DEFAULT_CONFIG_PATH
    args.config_name = DEFAULT_CONFIG_NAME
    args.overrides = []

    option_name = None

    for arg in argv:
        # Parse the value portion of --option value.
        if option_name is not None:
            option_value = arg
            setattr(args, option_name, option_value)
            option_name = None
            continue

        option_detected = False
        for option in SPECIAL_OPTIONS:
            arg_option, *arg_value = arg.split("=", 1)
            if arg_option != option:
                continue
            option_detected = True
            # Parse --option=value or --option value.
            option_name = arg_option[len("--") :].replace("-", "_")
            if len(arg_value) != 0:
                [option_value] = arg_value
                setattr(args, option_name, option_value)
                option_name = None
            break
        if option_detected:
            continue

        # All other arguments are overrides.
        args.overrides.append(arg)

    return args


def parse_args(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    argv_groups = split_argv_groups(argv, SPECIAL_OPTIONS)
    args_groups = [parse_argv_group(argv_group) for argv_group in argv_groups]

    # Merge all args into a single namespace containing lists.
    args = argparse.Namespace()
    for args_group in args_groups:
        for key, value in vars(args_group).items():
            if not hasattr(args, key):
                setattr(args, key, [])
            getattr(args, key).append(value)

    # args.config_path = [args.config_path for args in args_groups]
    # args.config_name = [args.config_name for args in args_groups]
    # args.overrides = [args.overrides for args in args_groups]

    return args


def main():
    args = parse_args()
    print(args.config_path)
    print(args.config_name)
    print("")

    for config_path, config_name, overrides in zip(
        args.config_path, args.config_name, args.overrides
    ):
        print("========================================")
        print(f"config_path: {config_path}")
        print(f"config_name: {config_name}")
        print(f"overrides: {overrides}")
        print("========================================")
        with initialize(version_base=None, config_path=os.path.relpath(config_path)):
            conf = compose(config_name=config_name, overrides=overrides)
        print(OmegaConf.to_yaml(conf), end="")
        print("========================================")
        print("")


if __name__ == "__main__":
    main()
