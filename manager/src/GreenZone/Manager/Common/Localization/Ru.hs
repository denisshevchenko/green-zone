{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module GreenZone.Manager.Common.Localization.Ru where

import           Data.Monoid                    ( (<>) )

import qualified Data.Map.Strict                as Map

import           GreenZone.Manager.Common.Types ( LocaleSentences,
                                                  Sentence (..) )

sentencesRu :: LocaleSentences
sentencesRu = Map.fromList
    [ (OkButtonLabel,                       "Ок")
    , (CancelButtonLabel,                   "Прервать")
    , (YesButtonLabel,                      "Да")
    , (NoButtonLabel,                       "Нет")
    , (FixButtonLabel,                      "Исправить")
    , (InvalidDataTitle,                    "Неверные данные")
    , (CancelInstallationTitle,             "Прервать установку?")
    , (CancelInstallationMessage,           "Вы уверены, что хотите прервать установку?")
    , (LetSGetStartedTitle,                 "Начнём!")
    , (LetSGetStartedMessage,               "В процессе установки вам будет задан ряд вопросов\n"
                                            <> "об оборудовании, о настройках NightScout, и т.п.\n"
                                            <> "Также вы должны будете предоставить ваш sudo-пароль,\n"
                                            <> "поскольку для работы GreenZone потребуется установка\n"
                                            <> "дополнительных программ.")
    , (IUnderstand,                         "Продолжить")
    , (IDontWantToContinue,                 "Прервать")
    , (NotificationAboutPumpTitle,          "Уведомление о помпе")
    , (NotificationAboutPumpMessage,        "В настоящий момент GreenZone поддерживает только помпу\n"
                                            <> "Medtronic MMT Paradigm Real Time 722.\n"
                                            <> "В будущем список поддерживаемых помп будет расширен.")
    , (NotificationAboutCGMTitle,           "Уведомление о CGM")
    , (NotificationAboutCGMMessage,         "В настоящий момент GreenZone поддерживает только CGM\n"
                                            <> "Medtronic MiniLink MMT-7703.\n"
                                            <> "В будущем список поддерживаемых CGM будет расширен.")
    , (NotificationAboutSBCTitle,           "Уведомление об SBC")
    , (NotificationAboutSBCMessage,         "Подразумевается, что GreenZone запущен на Intel Edison совместно с\n"
                                            <> "900 MHz Explorer Board.\n"
                                            <> "В будущем планируется возможность установки GreenZone на другие SBC.")
    , (PumpSerialNumberTitle,               "Серийный номер помпы")
    , (PumpSerialNumberMessage,             "Введите серийный номер вашей помпы (6 цифр)")
    , (NightScoutHostTitle,                 "NightScout сайт")
    , (NightScoutHostMessage,               "Укажите ваш сайт NightScout (например, https://yourname.herokuapp.com/)")
    , (NightScoutAPISecretTitle,            "NightScout API-секрет")
    , (NightScoutAPISecretMessage,          "Укажите ваш NightScout API-секрет (тот самый, который вы указали при\n"
                                            <> "регистрации вашего NightScout сайта)")
    , (InvalidPumpSerialNumberMessage,      "Серийный номер помпы должен состоять из 6 цифр.")
    , (InvalidNightScoutHostMessage,        "Это непохоже на адрес сайта NightScout.")
    , (InvalidNightScoutAPISecretMessage,   "NightScout API-секрет должен включать как минимум 12 символов.")
    , (MaxIOBTitle,                         "Максимальный IOB")
    , (MaxIOBMessage,                       "Введите значение максимального IOB (Insulin-on-Board), также известного\n"
                                            <> "как «активный инсулин» (целое положительное число).")
    , (InvalidMaxIOBMessage,                "Значение максимального IOB должно быть целым положительным числом.")
    , (EnabledLabel,                        "Активировать")
    , (DisabledLabel,                       "Не надо")
    , (AutoSenseTitle,                      "AutoSense")
    , (AutoSenseMessage,                    "Активировать автоматическую подстройку чувствительности?")
    , (AutoTuneTitle,                       "AutoTune")
    , (AutoTuneMessage,                     "Активировать автоматическую подстройку базальной скорости и ratios?")
    , (GreenZoneIsHereTitle,                "Другая GreenZone?")
    , (GreenZoneIsHereMessage,              "Похоже, GreenZone уже установлена на этом SBC.\n"
                                            <> "ВНИМАНИЕ: Если вы продолжите установку, предыдущая версия GreenZone будет удалена.")
    , (WelcomeTitle,                        "Добро пожаловать!")
    , (WelcomeMessage,                      "Эта программа установит GreenZone на ваш SBC.\n"
                                            <> "Пожалуйста, выберите язык, который будет использоваться во время установки.")
    , (ReConfigureLabel,                    "Перенастроить GreenZone")
    , (InstallLabel,                        "Установить GreenZone")
    , (DeleteLabel,                         "Удалить GreenZone")
    ]
