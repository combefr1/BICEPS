# Power users involved

- [Ochsenbein, David](mailto:david.ochsenbein@novartis.com)
- [Fidler, Matt](mailto:matt.fidler@novartis.com)
- [Combes, Francois](mailto:francois.combes@novartis.com)

# Introduction:

The pharmacometrics (PMX) community such as pharmaceutical industries, health authorities, and consultants uses High Performance Computing (HPC) platforms for analyzing or reviewing or clinical data and performing larger-scale computations where the results are intended for submission to health authorities. An HPC environment allows users to efficiently conduct these analyses. Instead of being executed locally , modelers submit their computations as “jobs” to the compute cluster. This permits and encourages parallelization (“splitting” the work across several cores). PMX-specific software benefits greatly from this parallelization, allowing to significantly reduce computation time, though consuming more immediate resources. Nevertheless, some analyses can last for weeks. Evaluating and improving the cluster efficiency is therefore crucial to allow a rapid execution of analyses. But it is rather difficult to characterize how long it takes for a model or analysis to run. Should analyst report an exceptionally long runtime as an issue to IT, (and therefore consume IT resources), or is it a normal behavior to expect from an efficient HPC? The analysis may be simply complex and time-consuming. How is efficiency measured? To our knowledge, there is no benchmark set of test cases that quantifies expectations about runtime, which could be used to monitor performance over time. We don’t really know what qualifies an efficient HPC platform in the realm of PMX activities.

# Objectives: 

The goal of our research proposal is to establish an anonymous map of HPC efficiency among industry, institutional and consulting partners to be able to evaluate HPC efficiency. We are presenting here the results of a pilot project, aiming at extending the BICEPS study to all participants of the PMX community. We invite the audience to register for the BICEPS study and participate in the code development

# Methods: 

For our preliminary analysis, we used 5 case studies (available from Monolix [1]): PKVK, arthritis, hcv, hiv, tgi and a real life example, a simultaneous PKPD model of ribociclib PK with absolute neutrophils count (PK-ANC) model with Bayesian estimation (Lu et al, 2021) . Parameter estimation was performed in triplicate, using either default or expert algorithm settings (overall higher number of iterations for estimation without auto-stop criteria). These criteria were used to represent typical process during model building and validation. To measure the influence of the cluster settings, several resource allocation was tested: 1 core with 1, 6, 12 and 16 threads; 2 cores, 6, threads and 4 cores, 3 or 12 threads. Each combination of algorithm settings and resource allocation was performed in triplicate. From each scenario were summarized metrics evaluating runtime, CPU and memory usage.

# Results: 

The five models tested were deployed successfully using Monolix Suite 2021R2 via R version 3.4.1 [2], utilizing a Novartis on-the-premises high performance computing environment (Red Hat Enterprise Linux version 7).

As expected, increasing the total number of threads (n*N) reduced the total runtime. CPU efficiency, defined as CPU_time/(run_time.number_of_CPU) decreased from 100% to 25% with parallelization .  CPU efficiency decreased with increasing the number of nodes, as more resources were pulled in for the same overall computation. Furthermore, with the same total threads, using parallelization led to a decrease in CPU efficiency and increase in pending time, with no significant improvement in runtime. This highlights the importance of proper resource scaling to prevent over-consumption, and thus impacting other HPC users. Finally, applying expert SAEM algorithm settings increased overall the resource consumption though the CPU efficiency drop was less pronounced.

# Conclusions: 

In this abstract, we are summarizing the results of a pilot project to establish a benchmark of the performance of the HPC platforms available to the PMX community. The script created to test HPC performance is currently being applied to the cluster of several academic and industrial partners. By extending this partnership with universities, pharmaceutical companies, consultants, software developers and health authorities, we will continue to map the performance of the partner’s cluster to increase the precision of the benchmark. Each participant would anonymously share the performances of their HPC via a set of analyses performed in Nonmem, Monolix and nlmixr2, and report the overall performances for each set of models. Each user would then be able to compare its performance versus the benchmark.  

