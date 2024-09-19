import hydra
import yaml
from omegaconf import DictConfig, OmegaConf


@hydra.main(version_base=None, config_path="conf")
def main(conf: DictConfig):
    # print(conf)
    print(yaml.dump(OmegaConf.to_container(conf, resolve=True)))


if __name__ == "__main__":
    main()
