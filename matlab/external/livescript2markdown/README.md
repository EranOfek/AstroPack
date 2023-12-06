# Live Script to Markdown Converter Toolbox
# Introduction

This toolbox provides a function `livescript2markdown` that allows you to convert your live scripts to markdown files. This function helps you to document your repositories.

# Requirements

   -  MATLAB R2020b or later 
   -  [livescript2markdown: MATLAB's live scripts to markdown](https://www.mathworks.com/matlabcentral/fileexchange/73993) addon (will be installed automatically with toolbox) 

# How to install
## Approach 1 (easy and fast)

Execute in MATLAB:

```matlab:Code(Display)
eval(webread('https://git.io/JYcyl'))
```

## Approach 2 (traditional)

Download **Live.Script.To.Markdown.Converter.mltbx** from the [latest release](https://github.com/roslovets/livescript2markdown/releases/latest) and open it in MATLAB.

## Approach 3 (boring)

Use **Add-Ons Explorer** in MATLAB to find and install **Live Script To Markdown Converter**.

# How to use

Use `livescript2markdown` function to convert mlx-script to md-file.

## Syntax

`livescript2markdown(mlxfile)`

`livescript2markdown(mlxfile,mdfile)`

`livescript2markdown(___,Name,Value)`

`mdfile = livescript2markdown(___)`

## Description

`livescript2markdown(mlxfile)` converts MATLAB live script (.mlx) to markdown text file (.md) in format suitable for GitHub or Qiita. Output md-file will be stored in current folder with the name of source mlx-file.

`livescript2markdown(mlxfile,mdfile)` allows you to specify full or relative path to the output markdown file

`livescript2markdown(___,Name,Value)` allows you to tune conversion process using one or more Name,Value pair arguments. Use this option with any of the input argument combinations in the previous syntaxes.

`mdfile = livescript2markdown(___)` returns full path to the output markdown file

## Input arguments

   -  `mlxfile` - absolute or relative path to the input live script file (.mlx). *Scalar string | char array* 
   -  `mdfile` - absolute or relative path to the output markdown text file (.md). *Scalar string | char array* 

### **Input Name-Value Pair Arguments**

   -  `'Format'`: It can be `'github'` (default) or `'qiita'.` Markdown file format 
   -  `'Png2jpeg'`: It can be `'false'` (default) or `'true'`. Enable to convert PNG images to JPEG images to compress 
   -  `'TableMaxWidth'`: max witdh of table in markdown (default: `20`) 
   -  `'FixLinks'`: convert file links with `open`, `winopen`, `cd` into suitable markdown format.  It can be `'true'` (default) or `'false'` 
   -  `'NormalizeLines'`: remove extra empty lines in generated file.  It can be `'true'` (default) or `'false'` 
   -  `'AddMention'`: mention this toolbox at the end of generated file.  It can be `'false'` (default) or `'true'` 

## Output arguments

   -  `mdfile` - absolute path to the output markdown text file (.md). *Scalar string* 

# Example

Convert documentation of this toolbox into README.md file

```matlab:Code(Display)
livescript2markdown('main/doc/GettingStarted.mlx', 'README.md', 'AddMention', true);
```

# Links

   -  [GitHub repository](https://github.com/roslovets/Live-Script-to-Markdown-Converter) 
   -  [File Exchange page](https://www.mathworks.com/matlabcentral/fileexchange/89516) 
   -  [More interesting projects](https://roslovets.github.io) 

# ⭐Show your support

Give a star if this project helped you.

***
*Generated from GettingStarted.mlx with [Live Script to Markdown Converter](https://github.com/roslovets/Live-Script-to-Markdown-Converter)*
