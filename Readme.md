[![Create and publish a Docker image](https://github.com/drejom/vscode-rbioc/actions/workflows/publish-to-github-package.yaml/badge.svg)](https://github.com/drejom/vscode-rbioc/actions/workflows/publish-to-github-package.yaml)
# Bioconductor on Apollo

This repository provides a Dockerfile that extends the official [Bioconductor Docker](https://bioconductor.org/help/docker/) image by adding some packages including the HPC job scheduler SLURM. GitHub actions build the image and push it to GitHub Packages.

## Additionally supported packages

[Bioconductor Docker](https://bioconductor.org/help/docker/) containers are based on [Rocker](https://rocker-project.org/) project images, which provide RStudio Server, a full featured IDE via a web browser. To the Rocker project's images, the Bioconductor developers add all the system dependencies required to support Bioconductor R libraries. We extend the container further by adding:

- ML libraries for transformers and convolutional neural networks
- System dependencies to support `bedr`, `ctrdata`, `monocle3`, `fnmate` and `datapasta`
- genomics tools like `sra-tools`, `bcftools` and `bedops`
- DNAnexus support (DX toolkit, dxfuse)
- SLURM
- JupyterLab
- VSCode LiveShare, R devcontainer [dependencies](https://github.com/microsoft/vscode-dev-containers/blob/main/containers/r/.devcontainer/devcontainer.json), miniconda

## Bioconductor version **3.19**

### Apollo

Build the container image for the HPC:

```sh
module load singularity
singularity pull -F /opt/singularity-images/rbioc/vscode-rbioc_3.19.sif docker://ghcr.io/drejom/vscode-rbioc:v2024-5-21
```
And launch on the HPC:

```sh
sbatch /opt/singularity-images/rbioc/rbioc319.job
```
### Gemini

Build the container image for the HPC:

```sh
module load singularity
singularity pull -F /packages/singularity/shared_cache/rbioc/vscode-rbioc_3.19.sif docker://ghcr.io/drejom/vscode-rbioc:v2024-5-21
```
And launch on the HPC:

```sh
# RStudio not supported on Gemini
#sbatch /packages/singularity/shared_cache/rbioc/rbioc319.job
# Use vscode tunnels
```

## Bioconductor version **3.18**

### Apollo

Build the container image for the HPC:

```sh
module load singularity
singularity pull -F /opt/singularity-images/rbioc/vscode-rbioc_3.18.sif docker://ghcr.io/drejom/vscode-rbioc:v2023-11-27
```
And launch on the HPC:

```sh
sbatch /opt/singularity-images/rbioc/rbioc318.job
```
### Gemini

Build the container image for the HPC:

```sh
module load singularity
singularity pull -F /packages/singularity/shared_cache/rbioc/vscode-rbioc_3.18.sif docker://ghcr.io/drejom/vscode-rbioc:v2023-11-27
```
And launch on the HPC:

```sh
#sbatch /packages/singularity/shared_cache/rbioc/rbioc318.job
```

## Bioconductor version **3.17**

### Apollo

Build the container image for the HPC:

```sh
module load singularity
singularity pull -F /opt/singularity-images/rbioc/vscode-rbioc_3.17.sif docker://ghcr.io/drejom/vscode-rbioc:v2023-9-26
```
And launch on the HPC:

```sh
sbatch /opt/singularity-images/rbioc/rbioc317.job
```
### Gemini

Build the container image for the HPC:

```sh
singularity pull -F /packages/singularity/shared_cache/rbioc/vscode-rbioc_3.17.sif docker://ghcr.io/drejom/vscode-rbioc:v2023-10-24
```
And launch on the HPC:

```sh
#sbatch /opt/singularity-images/rbioc/rbioc317.job
```

## Bioconductor version **3.16**

Build the container for the HPC:

```sh
module load singularity
singularity pull -F /opt/singularity-images/rbioc/vscode-rbioc_3.16.sif docker://ghcr.io/drejom/vscode-rbioc:v2023-1-8
```

And launch on the HPC:

```sh
sbatch /opt/singularity-images/rbioc/rbioc316.job
```

## Bioconductor version **3.15**

Build the container for the HPC:

```sh
module load singularity
singularity pull -F /opt/singularity-images/rbioc/vscode-rbioc_3.15.sif docker://ghcr.io/drejom/vscode-rbioc:v2022-10-14
```

And launch on the HPC:

```sh
sbatch /opt/singularity-images/rbioc/rbioc.job
```

# Docker

Build the Docker container locally:

```sh
docker buildx create --use
#docker buildx build --load --platform linux/amd64,linux/arm64 -t ghcr.io/drejom/vscode-rbioc:latest
docker buildx build --load --platform linux/amd64 -t ghcr.io/drejom/vscode-rbioc:latest --progress=plain . 2>&1 | tee build.log
```

Get a shell locally:

```sh
docker run -it --rm ghcr.io/drejom/vscode-rbioc:latest /bin/bash
```

