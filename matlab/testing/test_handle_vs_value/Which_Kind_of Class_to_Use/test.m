% https://www.mathworks.com/help/matlab/matlab_oop/which-kind-of-class-to-use.html


Examples of Value and Handle Classes

Handle and value classes are useful in different situations. For example, value classes enable you to create array classes that have the same behavior as MATLAB® numeric classes.

Representing Polynomials with Classes and Representing Structured Data with Classes provides examples of value classes.

Handle classes enable you to create objects that more than one function or object can share. Handle objects allow more complex interactions among objects because they allow objects to reference each other.

Implementing Linked Lists with Classes and Developing Classes That Work Together provides examples of a handle class.
When to Use Value Classes

Value class objects behave like normal MATLAB variables. A typical use of value classes is to define data structures. For example, suppose that you want to define a class to represent polynomials. This class can define a property to contain a list of coefficients for the polynomial. It can implement methods that enable you to perform various operations on the polynomial object. For example, implement addition and multiplication without converting the object to another class.

A value class is suitable because you can copy a polynomial object and have two objects that are identical representations of the same polynomial. For an example of value classes, see Subclasses of MATLAB Built-In Types.

For information on MATLAB pass-by-value semantics, see Avoid Unnecessary Copies of Data.
When to Use Handle Classes

Handle objects are useful in specialized circumstances where an object represents a physical object such as a graph or an external device rather than a mathematical object like a number or matrix. Handle objects are derivations of the handle class, which provides functionality such as events and listeners, destructor method, and support for dynamic properties.

Use a handle class when:

    No two instances of a class can have the same state, making it impossible to have exact copies. For example:

        A copy of a graphics object (such as a line) has a different position in its parents list of children than the object from which it was copied. Therefore, the two objects are not identical.

        Nodes in lists or trees having specific connectivity to other nodes — no two nodes can have the same connectivity.

    The class represents physical and unique objects like serial ports and printers.

    The class represents visible objects like graphics components.

    The class defines events and notifies listeners when an event occurs (notify is a handle class method).

    The class creates listeners by calling the handle class addlistener method.

    The class subclasses the dynamicprops class (a subclass of handle) so that instances can define dynamic properties.

    The class subclasses the matlab.mixin.SetGet class (a subclass of handle) so that it can implement a graphics object style set/get interface to access property values.

    You want to create a singleton class or a class in which you track the number of instances from within the constructor.

    Instances of a class cannot share state, such as nodes in a linked list.
	
	