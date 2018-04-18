module Msg exposing (..)

import Api
import Models.ApiError exposing (ApiError)
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.YmdDate as YmdDate
import Navigation
import Time


type Msg
    = NoOp
    | OnLocationChange Navigation.Location
    | TickMinute Time.Time
      -- Query Data related
    | OnGetHabitsAndHabitDataAndFrequencyStatsFailure ApiError
    | OnGetHabitsAndHabitDataAndFrequencyStatsSuccess Api.HabitsAndHabitDataAndFrequencyStats
      -- Add Habit related
    | OnOpenAddHabit
    | OnCancelAddHabit
    | OnSelectAddHabitKind Habit.HabitKind
    | OnAddHabitNameInput String
    | OnAddHabitDescriptionInput String
    | OnSelectAddGoodHabitTime Habit.HabitTime
    | OnAddHabitUnitNameSingularInput String
    | OnAddHabitUnitNamePluralInput String
    | OnAddHabitSelectFrequencyKind Habit.FrequencyKind
    | OnAddHabitTimesPerWeekInput String
    | OnAddHabitSpecificDayMondayInput String
    | OnAddHabitSpecificDayTuesdayInput String
    | OnAddHabitSpecificDayWednesdayInput String
    | OnAddHabitSpecificDayThursdayInput String
    | OnAddHabitSpecificDayFridayInput String
    | OnAddHabitSpecificDaySaturdayInput String
    | OnAddHabitSpecificDaySundayInput String
    | OnAddHabitTimesInput String
    | OnAddHabitDaysInput String
    | AddHabit Habit.CreateHabit
    | OnAddHabitFailure ApiError
    | OnAddHabitSuccess Habit.Habit
      -- Habit Amount related
    | OnHabitDataInput String String
    | SetHabitData YmdDate.YmdDate String (Maybe Int)
    | OnSetHabitDataFailure ApiError
    | OnSetHabitDataSuccess HabitData.HabitData
      -- Edit Habit related
    | OnHabitMouseEnter String
    | OnHabitMouseLeave
    | OnEditHabitIconClick Habit.Habit
    | OnEditHabitRevertAllToDefaults
    | OnSelectEditHabitKind Habit.HabitKind
    | OnEditHabitNameInput String
    | OnEditHabitDescriptionInput String
    | OnEditHabitUnitNameSingularInput String
    | OnEditHabitUnitNamePluralInput String
    | OnAbortEditHabitDialog
      -- History View related
    | OnToggleHistoryViewer
    | OnToggleTodayViewer
    | OnHistoryViewerDateInput String
    | OnHistoryViewerSelectYesterday
    | OnHistoryViewerSelectBeforeYesterday
    | OnHistoryViewerSelectDateInput
    | SetHistoryViewerSelectedDate YmdDate.YmdDate
    | OnGetPastFrequencyStatsFailure ApiError
    | OnGetPastFrequencyStatsSuccess Api.QueriedFrequencyStats
    | OnHistoryViewerChangeDate
    | OnHistoryViewerHabitDataInput YmdDate.YmdDate String String
