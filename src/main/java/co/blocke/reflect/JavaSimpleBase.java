package co.blocke.reflect;

@ClassAnno(name="dos") 
public class JavaSimpleBase<T> {

    private int two;
    public int getTwo(){ return two; }
    @FieldAnno(idx = 2)
    public void setTwo(int v) { two = v; }

    public T something;

    private int three = -10;
    @FieldAnno(idx = 99)
    public int getThree(){ return three; }
    public void setThree(int v) { three = v; }

    private int bogus = -1;
    public int getBogus(){ return bogus; }
    public void setBogus(int v) { bogus = v; }

}