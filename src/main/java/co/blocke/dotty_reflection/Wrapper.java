package co.blocke.dotty_reflection;

public class Wrapper<T> {
    private T item;
 
    public T getItem(){
        return item;
    }
 
    public void setItem(T item){
        this.item = item;
    }
}