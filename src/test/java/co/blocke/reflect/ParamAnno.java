package co.blocke.reflect;

@ClassAnno(name="Foom")
public class ParamAnno<T> {
  public ParamAnno() {}

  private String name;
  @FieldAnno(idx=1)
  public String getName() { return name; }
  public void setName(String n) { name = n; }

  private T age;
  public T getAge() { return age; }
  @FieldAnno(idx=2)
  public void setAge(T n) { age = n; }
}