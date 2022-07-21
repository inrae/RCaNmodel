package fr.cm.GUItablesViews;


import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.Action;
import fr.cm.canObjects.Component;
import fr.cm.canObjects.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

public class ActionTable extends Pane {

    final TableView<Action> table;
    ObservableList<Action> list;

    double width = 0.9 * Context.getWindowWidth();
    double height =  0.9 * Context.getWindowHeight();

    public ActionTable() {
        table = new TableView<>();
        table.setMinWidth(800.0);
        table.setEditable(true);

        TableColumn<Action, String> date = new TableColumn<>("Date");
        date.setCellValueFactory(new PropertyValueFactory<>("date"));
        date.setMinWidth(300.0);

        TableColumn<Action, String> comment = new TableColumn<>("Comment");
        comment.setCellValueFactory(new PropertyValueFactory<>("comment"));
        comment.setMinWidth(500.0);

        TableColumn<Action, String> commentAuthor = new TableColumn<>("Comment Author");
        commentAuthor.setCellFactory(TextFieldTableCell.forTableColumn());
        commentAuthor.setCellValueFactory(new PropertyValueFactory<>("commentAuthor"));
        commentAuthor.setMinWidth(500.0);
        commentAuthor.setEditable(true);
        commentAuthor.setOnEditCommit(
                event -> {
                    Action action = event.getTableView().getItems().get(event.getTablePosition().getRow());
                    action.setCommentAuthor(event.getNewValue());
                }
        );

        table.getColumns().add(date);
        table.getColumns().add(comment);
        table.getColumns().add(commentAuthor);

        list = FXCollections.observableArrayList(ProjectListsManager.getListOfActions());
        table.setItems(list);
        table.getSelectionModel().selectFirst();

        final Label title = new Label("System fluxes");
        title.setFont(ColorsAndFormats.titleFont);
        final VBox vbox = new VBox();
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(title, table);
        this.getChildren().addAll(vbox);
    }

}
