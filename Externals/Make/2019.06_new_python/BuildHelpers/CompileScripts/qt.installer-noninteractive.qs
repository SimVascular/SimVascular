// Emacs mode hint: -*- mode: JavaScript -*-

function Controller() {
    installer.autoRejectMessageBoxes();
    installer.installationFinished.connect(function() {
        gui.clickButton(buttons.NextButton,5000);
    })
}

Controller.prototype.WelcomePageCallback = function() {
    gui.clickButton(buttons.NextButton,5000);
}

Controller.prototype.CredentialsPageCallback = function() {
    gui.clickButton(buttons.NextButton,5000);
}

Controller.prototype.IntroductionPageCallback = function() {
    gui.clickButton(buttons.NextButton,5000);
}

Controller.prototype.TargetDirectoryPageCallback = function()
{
    gui.currentPageWidget().TargetDirectoryLineEdit.setText("REPLACEME_SV_TOP_BIN_DIR_QT");
    gui.clickButton(buttons.NextButton,5000);
}

Controller.prototype.ComponentSelectionPageCallback = function() {
    var widget = gui.currentPageWidget();
    
    widget.deselectAll();
    
    widget.selectComponent("qt.qt5.5113.gcc_64");
    widget.selectComponent("qt.tools.qtcreator");
    
    widget.selectComponent("qt.qt5.5113.qtscript");

    widget.selectComponent("qt.qt5.5113.qtcharts");
    widget.selectComponent("qt.qt5.5113.qtdatavis3d");
    widget.selectComponent("qt.qt5.5113.qtnetworkauth");
    widget.selectComponent("qt.qt5.5113.qtpurchasing");
    widget.selectComponent("qt.qt5.5113.qtremoteobjects");
    widget.selectComponent("qt.qt5.5113.qtvirtualkeyboard");
    widget.selectComponent("qt.qt5.5113.qtwebengine");
    widget.selectComponent("qt.qt5.5113.qtwebglplugin");

    gui.clickButton(buttons.NextButton,10000);
}

Controller.prototype.LicenseAgreementPageCallback = function() {
    gui.currentPageWidget().AcceptLicenseRadioButton.setChecked(true);
    gui.clickButton(buttons.NextButton,5000);
}

Controller.prototype.StartMenuDirectoryPageCallback = function() {
    gui.clickButton(buttons.NextButton,5000);
}

Controller.prototype.ReadyForInstallationPageCallback = function()
{
    gui.clickButton(buttons.NextButton,5000);
}

Controller.prototype.FinishedPageCallback = function() {
var checkBoxForm = gui.currentPageWidget().LaunchQtCreatorCheckBoxForm
if (checkBoxForm && checkBoxForm.launchQtCreatorCheckBox) {
    checkBoxForm.launchQtCreatorCheckBox.checked = false;
}
    gui.clickButton(buttons.FinishButton);
}
