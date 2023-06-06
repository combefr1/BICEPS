# type: ignore

# module load Python-bundle-Novartis
configfile: "config.yaml"

import os

USER=os.environ["USER"]
ROOT_FOLDER = os.getcwd()

GIT_URLS = config["git_urls"]

# default to small test
# this behavior can be overwritten by using 'snakemake ... --config test_type=big'
test_type = config["test_type"]
STAN_TEST = config["test_params"][test_type]["stan_test"]

envvars:
    "USER"

rule all:
    input:

rule get_tarball:
    output:
        tarball="resources/downloads/{tool}.tar.gz"
    params:
        git_url=lambda wc: GIT_URLS[wc.get("tool")]
    shell:
        "wget {params.git_url} -O {output.tarball}"

checkpoint unzip_tarball:
    output:
        git=directory("resources/{tool}/")
    input:
        tarball="resources/downloads/{tool}.tar.gz"
    wildcard_constraints:
        tool="\w+"
    shell:
        """
        mkdir -p {output}
        tar -xzf {input} --directory {output} --exclude=".git*"
        find "{output}" -mindepth 1 -maxdepth 1 -type d -exec sh -c 'mv "$0"/* "$1"' {{}} "{output}" \;
        
        # remove any empty subfolders
        find "{output}" -mindepth 1 -maxdepth 1 -type d -empty -delete
        """

rule run_monolix:
    input:
        "resources/monolix/helper_funs.R",
        "resources/monolix/templates/arthritis_project.mlxtran",
        "resources/monolix/templates/hcv_project.mlxtran",
        "resources/monolix/templates/hiv_project.mlxtran",
        "resources/monolix/templates/PKVK_project.mlxtran",
        "resources/monolix/templates/tgi_project.mlxtran",
        "resources/monolix/templates/model/arthritis_model.txt",
        "resources/monolix/templates/model/hcvNeumann98_model.txt",
        "resources/monolix/templates/model/hiv_model.txt",
        "resources/monolix/templates/model/PKVK_model.txt",
        "resources/monolix/templates/model/tgi_model.txt",
        "resources/monolix/templates/data/arthritis_data.txt",
        "resources/monolix/templates/data/hcv_data.txt",
        "resources/monolix/templates/data/hiv_data.txt",
        "resources/monolix/templates/data/PKVK_data.txt",
        "resources/monolix/templates/data/tgi_data.csv",
        script="resources/monolix/script.R"
    output:
        results="outputs/monolix/BICEPS_results.csv"
    log: "outputs/logs/monolix.log"
    threads: 1
    params:
        test_type=test_type
    shell:
        "module purge && "
        "module load R-Monolix/2021R2-0.6-gomkl-2019a-R-4.1.0 && "
        "Rscript -e 'source(\"{input.script}\"); main(test_type={\"params.test_type\"})' &> {log}"
