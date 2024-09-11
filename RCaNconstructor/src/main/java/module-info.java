module RCaNconstructor {

    requires javafx.controls;
    requires javafx.fxml;

    requires RCaller;
    requires poi;
    requires java.logging;
    requires commons-io;
    requires java.desktop;

    opens fr.cm.Main to javafx.graphics;
}
