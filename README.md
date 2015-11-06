# Meta-Eval

A tool for evaluating the accuracy of extracting metadata from research papers.

## Usage

    ./meta-eval.sh --input <dir> --truth <dir> --output <dir>
    
where

- `--input` points to a directory of predicted labels (see below for format description)
- `--truth` points to a directory of true labels (defaults to `src/main/resources/gold/acl`)
- `--output` points to a directory where the output report will be saved (defaults to `output`)
 
## Label Format

The input/truth directories should each contain a set of files named `<field>.txt` where `<field>` 
is the name of a metadata field, e.g. **titleExact** for the exact title string or **titleNormalized**
for the normalized title string. A metadata field can be multi-valued, e.g. for authors. Files with the
same name in the input/truth directories will be compared against each other to compute a precision/recall
number for that metric. The format of a label file is tab-separated:

    <paper-id>	<value1>	<value2>	...
    	
where any number of values can be listed, each separated by a tab.

Note that paper PDFs can be downloaded via 
`http://ai2-s2-pdfs.s3.amazonaws.com/<first-4-digits-of-paper-id>/<remainder-of-paper-id>.pdf`,
e.g. [http://ai2-s2-pdfs.s3.amazonaws.com/3bc4/6849083e55e690fb3e2b8e3a18b3adc19a32.pdf](http://ai2-s2-pdfs.s3.amazonaws.com/3bc4/6849083e55e690fb3e2b8e3a18b3adc19a32.pdf)

## Measurement Methodology

A precision/recall number will be computed for each paper that appears in either the predictions 
or the truth file for a field.  The truth file yields a set of string values for each paper, 
and the predictions file yields a set of string values.  The precision 
is the ratio of the size of the intersection between the true and predicted values to the number
of predicted values. The recall is the ratio of the size of the intersection to the number of
true values. Either precision or recall may be undefined if there are no predicted values or no true values.

The overall precision/recall number is the average precision/recall value over all papers in which the
precision/recall is defined.

## Output

The program creates a `PR.txt` file with the overall precision/recall number for each metadata field.
It also creates a `details` directory with information about individual examples contained in files named
`<field>-labels.txt`. The files have a tab-separated format:

    <paper-id>	<accuracy>	<type>	<value>

where each predicted/true value for a paper is listed on a separate line.

- `type` is **true** if the value exists in the truth file or **pred** if it exists in the predictions file
- `accuracy` is 1 if the value is in the intersection between truth and predictions and 0 otherwise
- `value` is the value of the field

Thus, **\[type=true, accuracy=0\]** indicates a recall error, while **\[type=pred, accuracy=0\]** indicates a precision error.

## Baseline

Truth files for `acl` and `dblp` datasets are in `src/main/resources/gold`

Prediction files for some baseline models (grobid, scienceParse, and scienceParse-highP) on those datasets 
are in `src/main/resources/baseline` You can generate goaccuracy numbers for a baseline model against a gold dataset with 

    ./meta-eval.sh --truth src/main/resources/gold/<dataset> --input src/main/resources/baseline/<dataset>/<model>
    
