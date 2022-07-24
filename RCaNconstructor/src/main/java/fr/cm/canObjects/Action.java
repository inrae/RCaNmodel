package fr.cm.canObjects;

import fr.cm.RCaNMain.Context;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

public class Action {

    final SimpleStringProperty date = new SimpleStringProperty();
    final SimpleStringProperty whichAction = new SimpleStringProperty();
    final SimpleStringProperty commentAuthor = new SimpleStringProperty();

    public Action(String whichAction){
        this.setDate(Context.getDate());
        this.setWhichAction(whichAction);
        this.setCommentAuthor("");
    }
    
    public Action(String whichAction, String commentAuthor){
        this.setDate(Context.getDate());
        this.setWhichAction(whichAction);
        this.setCommentAuthor(commentAuthor);
    }
    public Action(String date, String whichAction, String commentAuthor){
        this.setDate(Context.getDate());
        this.setWhichAction(whichAction);
        this.setCommentAuthor(commentAuthor);
    }

    public String getDate() {
        return date.get();
    }
    public StringProperty dateProperty() {
        return date;
    }
    public void setDate(String date) {
        this.date.set(date);
    }
    public String getWhichAction() {
        return whichAction.get();
    }
    public StringProperty whichActionProperty() {
        return whichAction;
    }
    public void setWhichAction(String whichAction) {
        this.whichAction.set(whichAction);
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
}
