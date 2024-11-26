from pathlib import Path

import hydra
from omegaconf import DictConfig, OmegaConf

config_path = str(Path(__file__).parent.joinpath("../conf").resolve())


@hydra.main(version_base=None, config_path=config_path)
def module_function(cfg: DictConfig):
    print(OmegaConf.to_yaml(cfg))
