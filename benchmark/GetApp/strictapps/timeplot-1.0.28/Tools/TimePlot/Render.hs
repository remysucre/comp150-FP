{-# LANGUAGE ParallelListComp #-}
module Tools.TimePlot.Render (
    dataToPlot
) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Event
import Control.Lens
import Data.Time
import Data.Colour
import Data.Colour.Names
import Data.Default
import Data.Maybe

import Tools.TimePlot.Types
import Tools.TimePlot.Plots

dataToPlot :: AxisData LocalTime -> (LocalTime,LocalTime) -> PlotData -> StackedLayout LocalTime
dataToPlot commonTimeAxis tr pd = dataToPlot' commonTimeAxis $ constrainTime tr pd

constrainTime :: (LocalTime,LocalTime) -> PlotData -> PlotData
constrainTime tr@(t0,t1) p@PlotBarsData{} = p {barsValues = filter (inRange tr . fst) (barsValues p)}
constrainTime tr@(t0,t1) p@PlotEventData{} = p {eventData = filter (any (inRange tr) . eventTimes) (eventData p)}
constrainTime tr@(t0,t1) p@PlotLinesData{} = p {linesData = map (filter (inRange tr . fst)) (linesData p)}
constrainTime tr@(t0,t1) p@PlotDotsData{} = p {dotsData = map (filter (inRange tr . fst)) (dotsData p)}

inRange (t0,t1) t = t>=t0 && t<=t1
eventTimes e = [eventStart e, eventEnd e]

dataToPlot' :: AxisData LocalTime -> PlotData -> StackedLayout LocalTime
dataToPlot' commonTimeAxis p@PlotBarsData{} = StackedLayout $ layoutWithTitle commonTimeAxis [plotBars plot] (plotName p) (length (barsTitles p) > 1)
  where plot = plot_bars_values      .~ barsValues p $
               plot_bars_item_styles .~ barsStyles p $
               plot_bars_style       .~ barsStyle  p $
               plot_bars_titles      .~ barsTitles p $
               ourPlotBars
dataToPlot' commonTimeAxis p@PlotEventData{} = StackedLayout $ layoutWithTitle commonTimeAxis [toPlot plot] (plotName p) False
  where plot = plot_event_data           .~ eventData p $
               plot_event_long_fillstyle .~ toFillStyle $
               plot_event_label          .~ toLabel     $
               def
        toFillStyle s = solidFillStyle . opaque $ fromMaybe lightgray (readColourName (statusColor s))
        toLabel     s = statusLabel s
dataToPlot' commonTimeAxis p@PlotLinesData{} = StackedLayout $ layoutWithTitle commonTimeAxis (map toPlot plots) (plotName p) (length (linesData p) > 1)
  where plots = [plot_lines_values .~ [vs] $
                 plot_lines_title  .~ title $
                 plot_lines_style  .~ lineStyle $
                 def
                 | vs <- linesData p
                 | title <- linesTitles p
                 | lineStyle <- linesStyles p]
dataToPlot' commonTimeAxis p@PlotDotsData{} = StackedLayout $ layoutWithTitle commonTimeAxis (map toPlot plots) (plotName p) (length (dotsData p) > 1)
  where plots = [plot_points_values .~ vs $
                 plot_points_style  .~ hollowCircles 4 1 color $
                 plot_points_title  .~ subtrack $
                 def
                 | subtrack <- dotsTitles p
                 | color <- dotsColors p
                 | vs <- dotsData p]

layoutWithTitle :: (PlotValue a, Show a) => AxisData LocalTime -> [Plot LocalTime a] -> String -> Bool -> Layout LocalTime a
layoutWithTitle commonTimeAxis plots name showLegend =
    layout_title .~ "" $
    layout_plots .~ plots $
    (if showLegend then id else (layout_legend .~ Nothing)) $
    layout_x_axis . laxis_generate .~ (\_ -> commonTimeAxis) $
    layout_y_axis . laxis_title .~ name $
    layout_margin .~ 0 $
    layout_grid_last .~ True $
    def

ourPlotBars :: (BarsPlotValue a) => PlotBars LocalTime a
ourPlotBars = plot_bars_spacing .~ BarsFixGap 0 0 $
              plot_bars_style   .~ BarsStacked    $
              plot_bars_alignment .~ BarsLeft     $
              def
