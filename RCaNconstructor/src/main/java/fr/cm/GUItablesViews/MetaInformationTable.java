package fr.cm.GUItablesViews;


import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.MetaElement;
import fr.cm.canObjects.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.Label;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.util.Callback;

public class MetaInformationTable extends Pane {

    double width = 0.9 * Context.getWindowWidth();
    double height =  0.9 * Context.getWindowHeight();

    final TableView<MetaElement> table;
    ObservableList<MetaElement> list;

    public MetaInformationTable() {
        super();

        table = new TableView<>();
        table.setPrefWidth(width);
        table.setPrefHeight(height);
        table.setEditable(true);

        TableColumn<MetaElement, String> metaName = new TableColumn<>("Field");
        metaName.setCellValueFactory(new PropertyValueFactory<>("metaName"));
        metaName.setEditable(false);
        metaName.setMinWidth(width/4);


        Callback cellFactoryMeta = new Callback<TableColumn, TableCell>() {
            @Override
            public TableCell call( TableColumn param) {
                final TableCell cell = new TableCell() {
                    private Text text;
                    @Override
                    public void updateItem(Object item, boolean empty) {
                        super.updateItem(item, empty);
                        if (!isEmpty()) {
                            String string = item.toString();
                            Font font;
                            if(string.startsWith("<")) {
                                font = Font.font("Verdana", FontWeight.BOLD, 20);
                                string = string.
                                        replace("<","").
                                        replace(">","");
                            }
                            else {
                                font = Font.font("Verdana", FontWeight.NORMAL, 15);
                            };
                            text= new Text(string);
                            text.setFont(font);
                            setGraphic(text);
                        }
                    }
                };
                return cell;
            }
        };
        metaName.setCellFactory(cellFactoryMeta);
        table.getColumns().add(metaName);
        
        TableColumn<MetaElement, String> metaContentProperty = new TableColumn<>("Your comments");
        metaContentProperty.setCellValueFactory(new PropertyValueFactory<>("metaContent"));
        metaContentProperty.setCellFactory(TextFieldTableCell.forTableColumn());
        metaContentProperty.setEditable(true);
        metaContentProperty.setMinWidth(width/3);
        metaContentProperty.setOnEditCommit(
                event -> {
                    MetaElement metaElement = event.getTableView().getItems().get(event.getTablePosition().getRow());
                    metaElement.setMetaContent(event.getNewValue());
                }
        );
        table.getColumns().add(metaContentProperty);

        TableColumn metaHint = new TableColumn<>("Hint");
        metaHint.setCellValueFactory(new PropertyValueFactory<MetaElement, String>("metaHint"));
        metaHint.setEditable(false);
        metaHint.setMinWidth(5 * width/12);

        Callback<TableColumn, TableCell> cellFactoryHint = new Callback<TableColumn, TableCell>() {
            @Override
            public TableCell call( TableColumn param) {
                final TableCell cell = new TableCell() {
                    private Text text;
                    @Override
                    public void updateItem(Object item, boolean empty) {
                        super.updateItem(item, empty);
                        if (!isEmpty()) {
                            text= new Text(item.toString());
                            Font font = Font.font("Verdana",FontPosture.ITALIC, 12);
                            text.setFont(font);
                            text.setWrappingWidth(5 * width/12);
                            setGraphic(text);
                        }
                    }
                };
                return cell;
            }
        };
        metaHint.setCellFactory(cellFactoryHint);

        table.getColumns().add(metaHint);

        list = FXCollections.observableArrayList(ProjectListsManager.getMetaInformation().getElements());
        table.setItems(list);
        table.getSelectionModel().selectFirst();

        final Label title = new Label("Meta Information");
        title.setFont(ColorsAndFormats.titleFont);
        final VBox vbox = new VBox();
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(title, table);
        this.getChildren().addAll(vbox);
    }
}
