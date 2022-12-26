import hydra
from omegaconf import DictConfig


@hydra.main(version_base=None, config_path="conf")
def main(conf: DictConfig):
    print(conf)


if __name__ == "__main__":
    main()
