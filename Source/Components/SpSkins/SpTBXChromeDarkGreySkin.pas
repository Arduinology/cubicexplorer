// Created by Marko Savolainen

unit SpTBXChromeDarkGreySkin;

interface

uses
  SpTBXSkins,
  Graphics, Windows, Controls;

type
  TSpTBXChromeSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect; Component:
        TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background, Borders:
        Boolean; Vertical: Boolean = False; ForceRectBorders: TAnchors = []);
        override;
  end;

implementation

{-------------------------------------------------------------------------------
  Fill Options
-------------------------------------------------------------------------------}
procedure TSpTBXChromeSkin.FillOptions;
begin
  SkinName:= 'Chrome (Dark Grey)';
  SkinAuthor:= 'Marko Savolainen';

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(1, $e8e8e8, $c8c8c8, $c8c8c8, $c8c8c8);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, $c8c8c8, clNone, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, $c2c2c2, $c2c2c2, clNone, clNone);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(3, $e0e0e0, $d0d0d0, $d0d0d0, $c0c0c0);

  Options(skncPanel, sknsNormal).Body.Fill(0, $c8c8c8, clNone, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, $c2c2c2, $c2c2c2, clNone, clNone);

  Options(skncPopup, sknsNormal).Body.Fill(0, $ededed, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $c2c2c2, $c2c2c2, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(1, $ededed, $c8c8c8, clNone, clNone);

  Options(skncSplitter, sknsNormal).Body.Fill(0, $c8c8c8, $c8c8c8, clNone, clNone);

  Options(skncWindow, sknsNormal).Borders.Fill(0, $ededed, $ededed, $c2c2c2, $c2c2c2);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(1, $e7e7e7, $d5d5d5, clNone, clNone);

  //Options(skncMenuBar, sknsNormal).Body.Fill(1, $ededed, $c8c8c8, $c8c8c8, $c8c8c8);

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $c2c2c2, $c8c8c8, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $c2c2c2, $c8c8c8, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $c2c2c2, $c8c8c8, clNone, clNone);

  //---- Buttons ----//

  //ToolbarItem
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(1, $eeeeee, $fbfbfb, clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(1, $ededed, $c8c8c8, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(2, $c2c2c2, $c8c8c8, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(1, $fcfcfc, $e7e7e7, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(2, $b0b0b0, $c8c8c8, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(1, $e9e9e9, $e4e4e4, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(2, $c2c2c2, $c8c8c8, clNone, clNone);
  Options(skncToolbarItem, sknsDisabled).TextColor := $a0a0a0;
  // MenuBarItem
  Options(skncMenuBarItem, sknsHotTrack).Body.Fill(0, $ededed, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsHotTrack).Borders.Fill(2, $dedede, $dedede, clNone, clNone);
  Options(skncMenuBarItem, sknsPushed).Body.Fill(0, $eeeeee, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsPushed).Borders.Fill(2, $c2c2c2, $c8c8c8, clNone, clNone);
  Options(skncMenuBarItem, sknsChecked).Body.Fill(1, $fcfcfc, $e7e7e7, clNone, clNone);
  Options(skncMenuBarItem, sknsChecked).Borders.Fill(2, $b0b0b0, $c8c8c8, clNone, clNone);
  Options(skncMenuBarItem, sknsCheckedAndHotTrack).Body.Fill(0, $e3e3e3, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsCheckedAndHotTrack).Borders.Fill(2, $a8a8a8, $c8c8c8, clNone, clNone);
  // MenuItem
  Options(skncMenuItem, sknsHotTrack).Body.Fill(0, $f9c9a9, $e7e7e7, clNone, clNone);
  Options(skncMenuItem, sknsHotTrack).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncMenuItem, sknsChecked).Body.Fill(1, $fcfcfc, $e7e7e7, clNone, clNone);
  Options(skncMenuItem, sknsChecked).Borders.Fill(2, $b0b0b0, $c8c8c8, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Body.Fill(0, $e3e3e3, clNone, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Borders.Fill(2, $a8a8a8, $c8c8c8, clNone, clNone);
  // Button
  Options(skncButton, sknsNormal).Body.Fill(1, $fefefe, $f7f7f7, clNone, clNone);
  Options(skncButton, sknsNormal).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncButton, sknsDisabled).Body.Fill(1, $ededed, $c8c8c8, clNone, clNone);
  Options(skncButton, sknsDisabled).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncButton, sknsDisabled).TextColor := $a0a0a0;
  Options(skncButton, sknsHotTrack).Body.Fill(1, $eeeeee, $fbfbfb, clNone, clNone);
  Options(skncButton, sknsHotTrack).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncButton, sknsPushed).Body.Fill(1, $ededed, $c8c8c8, clNone, clNone);
  Options(skncButton, sknsPushed).Borders.Fill(2, $c2c2c2, $c8c8c8, clNone, clNone);
  Options(skncButton, sknsChecked).Body.Fill(1, $fcfcfc, $e7e7e7, clNone, clNone);
  Options(skncButton, sknsChecked).Borders.Fill(2, $c2c2c2, $c8c8c8, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Body.Fill(1, $e9e9e9, $e4e4e4, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Borders.Fill(2, $c2c2c2, $c8c8c8, clNone, clNone);
  // List Item
  Options(skncListItem, sknsChecked).Body.Fill(0, $f9c9a9, $e7e7e7, clNone, clNone);
  Options(skncListItem, sknsHotTrack).Body.Fill(0, $f9c9a9, $e7e7e7, clNone, clNone);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Fill(0, $e3e3e3, clNone, clNone, clNone);
  // Check Box
  Options(skncCheckBox, sknsNormal).Body.Fill(1, $fefefe, $f7f7f7, clNone, clNone);
  Options(skncCheckBox, sknsNormal).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Body.Fill(1, $ededed, $c8c8c8, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).TextColor := $a0a0a0;
  Options(skncCheckBox, sknsHotTrack).Body.Fill(1, $eeeeee, $fbfbfb, clNone, clNone);
  Options(skncCheckBox, sknsHotTrack).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncCheckBox, sknsChecked).Body.Fill(1, $fefefe, $f7f7f7, clNone, clNone);
  Options(skncCheckBox, sknsChecked).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncCheckBox, sknsCheckedAndHotTrack).Body.Fill(1, $fefefe, $f7f7f7, clNone, clNone);
  Options(skncCheckBox, sknsCheckedAndHotTrack).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncCheckBox, sknsPushed).Body.Fill(1, $fefefe, $f7f7f7, clNone, clNone);
  Options(skncCheckBox, sknsPushed).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  // Radio Button
  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  
  // Edit Frame
  Options(skncEditFrame, sknsNormal).Borders.Fill(2, $e2e2e2, $e2e2e2, clNone, clNone);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(2, $d5d5d5, $d5d5d5, clNone, clNone);
  // Edit Button
  Options(skncEditButton, sknsHotTrack).Body.Fill(1, $fefefe, $f7f7f7, clNone, clNone);
  Options(skncEditButton, sknsHotTrack).Borders.Fill(0, $ededed, $ededed, clNone, clNone);
  Options(skncEditButton, sknsPushed).Body.Fill(1, $ededed, $c8c8c8, clNone, clNone);
  Options(skncEditButton, sknsPushed).Borders.Fill(0, $ededed, $ededed, clNone, clNone);
  Options(skncEditButton, sknsChecked).Body.Fill(1, $fcfcfc, $e7e7e7, clNone, clNone);
  Options(skncEditButton, sknsChecked).Borders.Fill(0, $ededed, $ededed, clNone, clNone);
  Options(skncEditButton, sknsCheckedAndHotTrack).Body.Fill(1, $e9e9e9, $e4e4e4, clNone, clNone);
  Options(skncEditButton, sknsCheckedAndHotTrack).Borders.Fill(0, $ededed, $ededed, clNone, clNone);

  //---- Tabs ----//
  //Options(skncTab, sknsNormal).Body.Fill(0, clNone, clNone, clNone, clNone);
  Options(skncTab, sknsNormal).Borders.Fill(2, $b8b8b8, clNone, clNone, clNone);
  Options(skncTab, sknsDisabled).Body.Fill(0, $e6e6e6, $e6e6e6, clNone, clNone);
  Options(skncTab, sknsDisabled).Borders.Fill(2, $ededed, $ededed, clNone, clNone);
  Options(skncTab, sknsHotTrack).Body.Fill(0, $c0c0c0, $c0c0c0, clNone, clNone);
  Options(skncTab, sknsHotTrack).Borders.Fill(2, $a0a0a0, $a0a0a0, clNone, clNone);
  Options(skncTab, sknsChecked).Body.Fill(0, $c8c8c8, $c8c8c8, clNone, clNone);
  Options(skncTab, sknsChecked).Borders.Fill(2, $989898, $989898, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(0, $c8c8c8, $c8c8c8, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(2, $989898, $989898, clNone, clNone);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, $C8C8C8, $b8b8b8, clNone, clNone);
  Options(skncTabToolbar, sknsNormal).Body.Fill(1, $c7c7c7, $b5b5b5, clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(1, $fefefe, $f7f7f7, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(2, $E3E3E3, $E3E3E3, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(1, $fcfcfc, $e7e7e7, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(2, $E3E3E3, $E3E3E3, clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(1, $fefefe, $f4f4f4, clNone, clNone);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(2, $dcdcdc, $dcdcdc, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(1, $f4f4f4, $fcfcfc, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(2, $dcdcdc, $dcdcdc, clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(0, $ededed, clNone, clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $e0e0e0, $e0e0e0, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(1, $eeeeee, $fbfbfb, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $ededed, $ededed, clNone, clNone);
  Options(skncHeader, sknsPushed).Body.Fill(1, $ededed, $c8c8c8, clNone, clNone);
  Options(skncHeader, sknsPushed).Borders.Fill(1, $c2c2c2, $c8c8c8, clNone, clNone);
end;

{-------------------------------------------------------------------------------
  Paint Background
-------------------------------------------------------------------------------}
procedure TSpTBXChromeSkin.PaintBackground(ACanvas: TCanvas; ARect: TRect;
    Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType;
    Background, Borders, Vertical: Boolean; ForceRectBorders: TAnchors);
begin
  if Component = skncTabBackground then
  begin
    SpFillRect(ACanvas, ARect, $C8C8C8);
    ACanvas.Pen.Style:= psSolid;
    ACanvas.Pen.Color:= $989898;
    if Borders then
    begin
      ACanvas.MoveTo(ARect.Left, ARect.Top);
      ACanvas.LineTo(ARect.Right, ARect.Top);
    end;
  end
  else if Component = skncTabToolbar then
  begin
    SpGradientFill(ACanvas, ARect, $b7b7b7, $a5a5a5, not Vertical);
    ACanvas.Pen.Style:= psSolid;
    ACanvas.Pen.Color:= $989898;
    ACanvas.MoveTo(ARect.Left, ARect.Top+0);
    ACanvas.LineTo(ARect.Right, ARect.Top+0);
    ACanvas.Pen.Color:= $aaaaaa;
    ACanvas.MoveTo(ARect.Left, ARect.Top+1);
    ACanvas.LineTo(ARect.Right, ARect.Top+1);
  end
  else
  inherited;
end;

{##############################################################################}

initialization
  SkinManager.SkinsList.AddSkin('Chrome (Dark Grey)', TSpTBXChromeSkin);

end.
