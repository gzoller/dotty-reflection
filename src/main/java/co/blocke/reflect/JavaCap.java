package co.blocke.reflect;

import com.mypkg.*;
import java.util.*;

public class JavaCap<T> extends SJCaptureJava {

    private Optional<Optional<String>> name;
    public Optional<Optional<String>> getName() { return name; }
    public void setName(Optional<Optional<String>> n) { name = n; }

    private String foo;
    public String getFoo() { return foo; }
    public void setFoo(String f){ foo = f; }

    public Object anything;
    public Object getAnything() { return anything; }
    public void setAnything(Object f){ anything = f; }

    public T mystery;
    public T getMystery() { return mystery; }
    public void setMystery(T f){ mystery = f; }

    public Optional<T> complex;
    public Optional<T> getComplex() { return complex; }
    public void setComplex(Optional<T> f){ complex = f; }
  }

  /*
  Test:

  1) Parameterized class *
  2) Option[T]
  3) naked (non-parameterized) Object-typed field *
  */