library(shinytest2)
library(RCaNconstructor)
# devtools::load_all("../../")
addNode <- function(app, id, label, Inside, cmd = "addNode"){
  if (cmd == "addNode"){
    app$run_js(paste0('var network = HTMLWidgets.find("#visnetwork-networkvizproxy");
             network.network.body.data.nodes.add([{id: "', id,'", label: "', label, '", Inside: ', Inside, '}]);'))
    
  } else if (cmd == "editNode"){
    app$run_js(paste0('var network = HTMLWidgets.find("#visnetwork-networkvizproxy");
             network.network.body.data.nodes.update([{id: "', id,'", label: "', label, '", Inside: ', Inside, '}]);'))
  } else{
    app$run_js(paste0('var network = HTMLWidgets.find("#visnetwork-networkvizproxy");
             network.network.body.data.nodes.remove("', id,'");'))
  }
  app$set_inputs(`visnetwork-networkvizproxy_graphChange` = list(id = id,
                                                                 label = label ,
                                                                 cmd = cmd,
                                                                 Inside = Inside), allow_no_input_binding_ = TRUE)
}



addEdge <- function(app, Trophic, from, to , cmd = "addEdge"){
  id = label = paste(from, to, sep = "_")
  if (cmd == "addEdge"){
    app$run_js(paste0('var network = HTMLWidgets.find("#visnetwork-networkvizproxy");
             network.network.body.data.edges.add([{id: "', id,'", label: "', label, '", from: "', from, '", to: "', to, '", Trophic: ', Trophic, '}]);'))
    
  } else if (cmd == "editNode"){
    app$run_js(paste0('var network = HTMLWidgets.find("#visnetwork-networkvizproxy");
             network.network.body.data.edges.update([{id: "', id,'", label: "', label, '", from: "', from, '", to: "', to, '", Trophic: ', Trophic, '}]);'))
  } else{
    app$run_js(paste0('var network = HTMLWidgets.find("#visnetwork-networkvizproxy");
             network.network.body.data.edges.remove("', id,'");'))
  }
  app$set_inputs(`visnetwork-networkvizproxy_graphChange` = list(id = id,
                                                                 label = label ,
                                                                 from = from,
                                                                 to = to,
                                                                 cmd = cmd,
                                                                 Trophic = Trophic), allow_no_input_binding_ = TRUE)
}

test_that("{shinytest2} recording: visnetwork", {
  app <- AppDriver$new(launchConstructor(),
                       name = "visnetwork", 
                       height = 873, 
                       width = 909, 
                       variant = platform_variant())
  app$set_inputs(`visnetwork-shinyjs-delay-b0c7cb97f7e886650b37232cde174d57` = 500,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(mainpanel = "Edit TrophicNetwork")
  app$set_inputs(`visnetwork-shinyjs-delay-8afca9e24db20ba572cad8934d0d631b` = 500,
                 allow_no_input_binding_ = TRUE)
  
  
  addNode(app, "new", "new", 1, "addNode")
  addNode(app, "new2", "new2", 1, "addNode")
  addNode(app, "new3", "new3", 1, "addNode")
  addNode(app, "new3", "new4", 0, "editNode")
  # app$set_inputs(`nodeSelectvisnetwork-networkvizproxy` = character(0))
  # app$set_inputs(`selectedByvisnetwork-networkvizproxy` = character(0))
  addEdge(app, 1, "new", "new2", cmd = "addEdge")
  addEdge(app, 1, "new", "new", cmd = "addEdge")
  addEdge(app, 1, "new2", "new2", cmd = "addEdge")
  addEdge(app, 0, "new2", "new2", cmd = "editEdge")
  
  app$set_inputs(editpanel = "Components")
  app$expect_values(export = c("fluxes", "components", "timeline"))
})


test_that("{shinytest2} recording: componentsfluxes", {
  app <- AppDriver$new(launchConstructor(),
                       name = "componentsfluxes",
                       height = 873, 
                       width = 909,
                       variant = platform_variant())
  app$set_inputs(`visnetwork-shinyjs-delay-b0c7cb97f7e886650b37232cde174d57` = 500,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(mainpanel = "Edit TrophicNetwork")
  app$set_inputs(`visnetwork-shinyjs-delay-8afca9e24db20ba572cad8934d0d631b` = 500,
                 allow_no_input_binding_ = TRUE)
  
  
  addNode(app, "new", "new", 1, "addNode")
  addNode(app, "new2", "new2", 1, "addNode")
  addEdge(app, 1, "new", "new", cmd = "addEdge")
  addEdge(app, 1, "new2", "new2", cmd = "addEdge")
  app$wait_for_idle()
  app$set_inputs(editpanel = "Components")
  app$wait_for_idle()
  app$run_js('var table = HTMLWidgets.find("#components-tableedit").hot;
             table.setDataAtCell(0,0,"new3");
             table.setDataAtCell(0,1,false);
             table.setDataAtCell(0,2, 0.001);')
  app$wait_for_idle()
  app$click("components-ok")
  app$wait_for_idle()
  app$set_inputs(editpanel = "Fluxes")
  app$wait_for_idle()
  #app$get_values(export=c("components","fluxes"))
  app$expect_values(export = c("components", "timeline"))
  app$run_js('var table = HTMLWidgets.find("#fluxes-tableedit").hot;
             table.setDataAtCell(0,0,"new3_new2");
             table.setDataAtCell(0,1,"new3");
             table.setDataAtCell(0,2, "new2");
             table.setDataAtCell(0,3, false);')
  app$wait_for_idle()
  app$click("fluxes-ok")
  app$wait_for_idle()
  app$set_inputs(editpanel = "Components")
  app$wait_for_idle()
  # app$get_values(export=c("components","fluxes"))
  app$expect_values(export = c("fluxes", "timeline"))
})



test_that("{shinytest2} recording: loadfile", {
  app <- AppDriver$new(launchConstructor(),
                       variant = platform_variant(),
                       name = "loadfile",
                       height = 873, 
                       width = 909,
                       shiny_args = list(test.mode = TRUE))
  app$click("files-open")
  app$upload_file(`files-loadname` = system.file("extdata",
                                                 "CaN_template_mini.xlsx", package = "RCaNmodel"))
  app$click("files-okload")
  app$wait_for_idle()
  app$set_inputs(mainpanel = "Edit TrophicNetwork")
  app$set_inputs(editpanel = "Components")
  app$wait_for_idle()
  app$expect_values(export=c("components",
                             "timeline",
                             "fluxes",
                             "constraints"))
})


test_that("{shinytest2} recording: importobs", {
  app <- AppDriver$new(launchConstructor(),
                       name = "importobs",
                       height = 643, 
                       width = 906,
                       variant = platform_variant())
  app$set_inputs(`visnetwork-shinyjs-delay-9da50a375729c9b2344463f26fc34234` = 500, 
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(mainpanel = "Observations")
  app$click("tabmetaobs-add")
  app$set_inputs(`tabmetaobs-importid-file-na_label` = ",NA", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-file-dec` = ".", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-file-encoding` = "UTF-8", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-googlesheets-link` = "", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-url-link` = "", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-update-format` = "%Y-%m-%d", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-update-origin` = "1970-01-01", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-update-dec` = ".", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-file-skip_rows` = 0, wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-from` = "file", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-file-dropdown_params_dropmenu` = FALSE, wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-update-settings_dropmenu` = FALSE, wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-file-sheet` = character(0), wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-tabs-mode` = "import", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-tabs-import` = "file", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-file-preview_data` = TRUE, wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-copypaste-data_pasted` = "", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-copypaste-name` = "", wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-hidden` = "03e87e631056c5863738e7a8", wait_ = FALSE)
  app$wait_for_idle()
  app$upload_file(`tabmetaobs-importid-file-file` = system.file("extdata",
                                                                "CaN_template_mini.xlsx", package = "RCaNmodel"))
  app$set_inputs(`tabmetaobs-importid-file-sheet_open` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(`tabmetaobs-importid-file-sheet` = "Input time-series")
  app$set_inputs(`tabmetaobs-importid-file-sheet_open` = FALSE, allow_no_input_binding_ = TRUE)
  app$click("tabmetaobs-importid-confirm")
  app$click("tabmetaobs-remove")
  app$wait_for_idle()
  app$set_inputs(`tabmetaobs-idremove` = "HerbZooplankton_Biomass")
  app$wait_for_value(input = "tabmetaobs-okremove")
  app$click("tabmetaobs-okremove")
  app$click("tabmetaobs-ok")
  app$wait_for_idle()
  app$set_inputs(editobservations = "Time Series")
  app$set_inputs(`visnetwork-shinyjs-delay-716b0ec823751995d02c1ff782f1a8c0` = 500, 
                 allow_no_input_binding_ = TRUE)
  app$wait_for_idle()
  app$expect_values(export=c("metaobs",
                             "timeline",
                             "observations"))
  app$run_js('var table = HTMLWidgets.find("#obs-tableedit").hot;
             table.setDataAtCell(0,0,99999);
             table.setDataAtCell(1,1,99999);;
             table.setDataAtCell(0,1,"");')
  app$wait_for_idle()
  app$click("obs-ok")
  app$wait_for_idle()
  app$set_inputs(mainpanel = "Edit TrophicNetwork")
  app$wait_for_idle()
  app$expect_values(export=c("metaobs",
                             "timeline",
                             "observations"))
})





test_that("{shinytest2} recording: constraints", {
  app <- AppDriver$new(launchConstructor(),
                       name = "constraints", 
                       height = 873, 
                       width = 909, 
                       variant = platform_variant())
  app$click("files-open")
  app$upload_file(`files-loadname` = system.file("extdata",
                                                 "CaN_template_mini.xlsx", package = "RCaNmodel"))
  app$click("files-okload")
  app$wait_for_idle()
  
  app$set_inputs(`visnetwork-shinyjs-delay-b0c7cb97f7e886650b37232cde174d57` = 500,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(mainpanel = "Constraints")
  app$set_inputs(editconstraints = "Add/Edit Constraints")
  app$set_inputs(`constreditor-newname` = "test")
  app$set_inputs(`constreditor-components` = "HerbZooplankton")
  app$set_inputs(`constreditor-comparisons` = "+")
  app$set_inputs(`constreditor-components` = "OmniZooplankton")
  app$set_inputs(`constreditor-comparisons` = ">=")
  app$set_inputs(`constreditor-obs` = "PrimaryProduction")
  app$click("constreditor-ok")
  app$wait_for_idle()
  app$set_inputs(`constreditor-constraintselect` = "C01")
  app$wait_for_idle()
  app$set_inputs(`constreditor-equations` = "4")
  app$wait_for_idle()
  app$set_inputs(`constreditor-comparisons` = "*")
  app$wait_for_idle()
  app$set_inputs(`constreditor-numbers` = "0")
  app$wait_for_idle()
  app$set_inputs(`constreditor-numbers` = ".")
  app$wait_for_idle()
  app$set_inputs(`constreditor-numbers` = "9")
  app$wait_for_idle()
  app$click("constreditor-ok")
  app$wait_for_idle()
  app$set_inputs(editconstraints = "View Constraints")
  app$wait_for_idle()
  app$expect_values(export=c("constraints", "timeline"))
  app$run_js('var table = HTMLWidgets.find("#tabconstraints-tableedit").hot;
             table.setDataAtCell(0,0,"C01bis");
             table.setDataAtCell(0,1,"F01+F02*0.9<=PrimaryProductio*1.3");
             ')
  app$click("tabconstraints-checkvalid")
  app$get_screenshot()
  app$run_js('var table = HTMLWidgets.find("#tabconstraints-tableedit").hot;
             table.setDataAtCell(0,0,"C01bis");
             table.setDataAtCell(0,1,"F01+F02*0.9<=PrimaryProduction*1.3");
             ')
  app$click("tabconstraints-checkvalid")
  app$get_screenshot()
  app$run_js('var table = HTMLWidgets.find("#tabconstraints-tableedit").hot;
             table.setDataAtCell(2,3,false);
             ')
  app$click("tabconstraints-ok")
  app$wait_for_idle()
  app$set_inputs(editconstraints = "Add/Edit Constraints")
  app$wait_for_idle()
  app$expect_values(export=c("constraints", "timeline"))
  
})

