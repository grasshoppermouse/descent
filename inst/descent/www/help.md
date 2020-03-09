
<style> 
  th, td { 
  padding: 10px; 
  }
</style> 

By [Ed Hagen](https://anthro.vancouver.wsu.edu/people/hagen)

<font color=#ff3333>D<small>ESCENT</small></font> provides a user-friendly interface to many functions of the [GENLIB](https://CRAN.R-project.org/package=GENLIB) package, which computes a variety of statistics about consanguineal kin from a population genealogy, including kinship coefficients between each member of the population and inbreeding coefficient for each member of the population. <font color=#ff3333>D<small>ESCENT</small></font> also checks for common errors in geneology files, and can compute the average relatedness of population subgroups (e.g. households). These statistics can then be used in other statistical analyses; for example, to determine the importance of various types of kinship for food sharing, political alliances, etc.

## File format

None of <font color=#ff3333>D<small>ESCENT'S</small></font> functions are available until a file is opened. Because <font color=#ff3333>D<small>ESCENT</small></font> is currently in a pre 1.0 release, make a backup of your genealogy files before opening them in <font color=#ff3333>D<small>ESCENT</small></font>.

The input file format is a simple text file, with each column delimited by commas, tabs, or semicolons. Each line of the file corresponds to a single member of the population (an 'ego'). Each line must contain a minimum of four fields (columns), separated by delimiters:

EGO ID *tab* FATHER ID *tab* MOTHER ID *tab* SEX

You do not need to use these specific column names, nor do they need to be in this order. <font color=#ff3333>D<small>ESCENT</small></font> can often guess which column is which, or they can be assigned manually.

Each member of the population must be assigned a unique Ego ID. Ego's biological father and mother are identified with their unique ID codes, or by a 'Missing' code if their identities are unknown (see below). Ego's sex must also be specified. An optional fifth field, LIVING, denoting whether EGO is alive or dead, can be included. The first line of the file can optionally contain column header labels (e.g., EGO, FATHER, etc.). Thus, the first four lines of a typical input file might look like this:

EGO      | FATHER   | MOTHER     | SEX  | LIVING
-------------- | ----------- | --------------- | ------ | ------
Kennedy John  | Kennedy Joe | Fitzgerald Rose | male  | No
Kennedy Robert | Kennedy Joe | Fitzgerald Rose | male  | No
Kennedy Eunice | Kennedy Joe | Fitzgerald Rose | female | Yes

The underlying [GENLIB](https://CRAN.R-project.org/package=GENLIB) package requires numeric id codes. If you use non-numeric ids, <font color=#ff3333>D<small>ESCENT</small></font> will convert them to numeric under the hood, but usually display your id codes rather than the converted codes. The only place where this is not possible is the pedigree plots, which currently only display numeric codes. To avoid this and other possible conflicts with the GENLIB package, use numeric id codes. <font color=#ff3333>D<small>ESCENT</small></font> can export a file with all id codes and column headers and values converted to GENLIB format.

<!-- average coefficient of relatedness of each member to the entire population, average coefficient of relatedness of each member to their consanguineal kin, number of kin of each population member, patri- and matrilineages, consanguineal connections between members of the population, and number of kin in particular categories (e.g., sisters) for each member of the population -->

## Errors

To activate <font color=#ff3333>D<small>ESCENT</small>'s</font> functions, you must click the **Error** tab. If your file contains errors such as non-unique ego ID's, female fathers, or male mothers, a table of each row with an error will be displayed that indicates the type of error.

If there are any instances of incest between very close relatives, e.g., brother-sister incest, these will be flagged as warnings since they might indicate an entry error.

Currently, you cannot fix errors or otherwise edit your file in <font color=#ff3333>D<small>ESCENT</small></font>. You must edit it in another text editing program and then re-upload it.

Once an error-free file is uploaded, additional tabs will appear.

## Summary

Displays basic information about the genealogy, including numbers of egos, males, females, and founders, and a histogram of the number of egos at each genealogical depth.

## Kin

Computes matrix of coefficients of kinship, coefficients of relation, inbreeding coefficients, and plots distributions of the sizes of matrilineages and patrilineages.

## Pedigree

Provides pedigree plots for any ego, and also for any matrilineage and patrilineage.
