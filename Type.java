/***
 * Type class and its subclasses: 
 * ErrorType, IntegerType, BooleanType, VoidType, StringType, FuncType,
 * StructType, StructDefType
 ***/
abstract public class Type {

    /***
     * default constructor
     ***/
    public Type() {
    }

    /***
     * every subclass must provide a toString method and an equals method
     ***/
    abstract public String toString();
    abstract public boolean equals(Type t);

    /***
     * default methods for "isXXXType"
     ***/
    public boolean isErrorType() {
        return false;
    }

    public boolean isIntegerType() {
        return false;
    }

    public boolean isBooleanType() {
        return false;
    }

    public boolean isVoidType() {
        return false;
    }
    
    public boolean isStringType() {
        return false;
    }

    public boolean isFuncType() {
        return false;
    }

    public boolean isStructType() {
        return false;
    }
    
    public boolean isStructDefType() {
        return false;
    }
}

// **********************************************************************
//   ErrorType
// **********************************************************************
class ErrorType extends Type {

    public boolean isErrorType() {
        return true;
    }

    public boolean equals(Type t) {
        return t.isErrorType();
    }

    public String toString() {
        return "error";
    }
}

// **********************************************************************
//   IntegerType
// **********************************************************************
class IntegerType extends Type {

    public boolean isIntegerType() {
        return true;
    }

    public boolean equals(Type t) {
        return t.isIntegerType();
    }

    public String toString() {
        return "integer";
    }
}

// **********************************************************************
//   BooleanType
// **********************************************************************
class BooleanType extends Type {

    public boolean isBooleanType() {
        return true;
    }

    public boolean equals(Type t) {
        return t.isBooleanType();
    }

    public String toString() {
        return "boolean";
    }
}

// **********************************************************************
//   VoidType
// **********************************************************************
class VoidType extends Type {

    public boolean isVoidType() {
        return true;
    }

    public boolean equals(Type t) {
        return t.isVoidType();
    }

    public String toString() {
        return "void";
    }
}

// **********************************************************************
//   StringType
// **********************************************************************
class StringType extends Type {

    public boolean isStringType() {
        return true;
    }

    public boolean equals(Type t) {
        return t.isStringType();
    }

    public String toString() {
        return "String";
    }
}

// **********************************************************************
//   FuncType
// **********************************************************************
class FuncType extends Type {

    public boolean isFuncType() {
        return true;
    }

    public boolean equals(Type t) {
        return t.isFuncType();
    }

    public String toString() {
        return "function";
    }
}

// **********************************************************************
//   StructType
// **********************************************************************
class StructType extends Type {
    private IdNode myId;
    
    public StructType(IdNode id) {
        myId = id;
    }
    
    public boolean isStructType() {
        return true;
    }

    public boolean equals(Type t) {
        return t.isStructType();
    }

    public String toString() {
        return myId.name();
    }
}

// **********************************************************************
//   StructDefType
// **********************************************************************
class StructDefType extends Type {

    public boolean isStructDefType() {
        return true;
    }

    public boolean equals(Type t) {
        return t.isStructDefType();
    }

    public String toString() {
        return "struct";
    }
}
