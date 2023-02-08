package fr.cm.objects;

import fr.cm.Main.Context;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

public class TimeLine {
// ---------------------------------
    final SimpleStringProperty date = new SimpleStringProperty();
    final SimpleStringProperty whichTimeLine = new SimpleStringProperty();
    final SimpleStringProperty commentAuthor = new SimpleStringProperty();

    public TimeLine(String whichTimeLine){
        this.setDate(Context.getDate());
        this.setWhichTimeLines(whichTimeLine);
        this.setCommentAuthor("");
    }
    
    public TimeLine(String whichTimeLine, String commentAuthor){
        this.setDate(Context.getDate());
        this.setWhichTimeLines(whichTimeLine);
        this.setCommentAuthor(commentAuthor);
    }
    public TimeLine(String date, String whichTimeLine, String commentAuthor){
        this.setDate(date);
        this.setWhichTimeLines(whichTimeLine);
        this.setCommentAuthor(commentAuthor);
    }

    public String getDate() {

        return date.get();
    }
    public StringProperty dateProperty() {

        return date;
    }
    private void setDate(String date) {

        this.date.set(date);
    }
    public String getWhichTimeLines() {

        return whichTimeLine.get();
    }
    public StringProperty whichTimeLineProperty() {

        return whichTimeLine;
    }
    public void setWhichTimeLines(String whichTimeLine) {

        this.whichTimeLine.set(whichTimeLine);
    }
    public String getCommentAuthor() {

        return commentAuthor.get();
    }
    public StringProperty commentAuthorProperty() {
        return commentAuthor;
    }
    public void setCommentAuthor(String commentAuthor) {

        this.commentAuthor.set(commentAuthor);
    }

    public void print(){
        System.out.println(this.getDate() + " " + this.getWhichTimeLines()+ " "+this.getCommentAuthor());
    }
}
