{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.HashMap.Strict as HM
import           Universum

import           Options             (Args (..), getOptions)
import           Types               (Response, SearchEntry (..))
import           Web                 (runStubApp)

resp :: HashMap String Response
resp = HM.fromList
    [ ( "google/москва"
      , [ SearchEntry "https://ru.wikipedia.org/wiki/Москва" "Москва — Википедия" "google"
        , SearchEntry "https://tonkosti.ru/Москва" "Москва — отдых, экскурсии, музеи, кухня и шоппинг ..." "google"
        , SearchEntry "https://www.tripadvisor.ru" "Москва 2017: все самое лучшее для туристов - TripAdvisor" "google"
        ] ),
      ( "yandex/москва"
      , [ SearchEntry "https://ru.wikipedia.org/wiki/Москва" "Москва — Википедия" "yandex"
        , SearchEntry "yandex.ru/maps/Москва" "Москва на карте России" "yandex"
        , SearchEntry "vk.com/mos" "Город Москва | ВКонтакте" "yandex"
        ] ),
      ( "google/java"
      , [ SearchEntry "https://java.com/ru/download/" "Загрузить бесплатное программное обеспечение Java" "google"
        , SearchEntry "https://www.java.com/ru/" "Java и вы: java.com" "google"
        , SearchEntry "https://ru.wikipedia.org/wiki/Java" "Java — Википедия" "google"
        ] ),
      ( "yandex/java"
      , [ SearchEntry "https://auto.ru" "Купить мотоцикл Jawa" "yandex"
        ] )
    ]

main :: IO ()
main = do
    Args {..} <- getOptions
    runStubApp port respTL resp
