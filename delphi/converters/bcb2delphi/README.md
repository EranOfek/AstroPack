

### C++ to Delphi converter

http://copycat.fr/wordpress/porting-old-cbuilder-software-to-new-versions/


In the same line of thought, I recently wrote a perl script to convert C++ Builder code to Delphi. 
Why? Well, C++Builder always was buggier and slower to compile than its Delphi counterpart, 
and this is still true in the latest releases. We came across a show-stopper bug in C++Builder 
XE6 that made us decide to perform an all-out conversion to Delphi.

As you would expect with this sort of script, it’s imperfect. 
There will be a few changes to make by hand in your code, and there are a few language 
structures that are not handled (the ternary operator for instance). 
There are also occasionnal goofs, and the comments outside the method 
implementations (or in the .H) are stripped, but overall, it should save you countless 
hours of manual converting if you were to decide to take this route.

We have used it on a major project (over 1200 forms) and haven’t had any significant problems yet.

You are free to use it as you please, but you are asked to provide us with any changes you make 
and let us know if it has been helpful to you. Friendly links would also be appreciated!

Here it is : http://copycat.fr/amember/content/f/id/24/

