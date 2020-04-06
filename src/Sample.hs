{-# LANGUAGE OverloadedStrings #-}

module Sample where

import Game
import qualified Component.NovelTitle as Title
import qualified Component.NovelBody as Body

title :: NovelGame ()
title = do
    loadComponents Title.simple
    updateSrc "title_bg" "test/img/bg.png"
    updateSrc "title_logo" "test/img/bg.png"
    enableOption "option1" "はじめから" start
    enableOption "option2" "つづきから" load
    idle

start :: NovelGame ()
start = do
    loadComponents Body.simple
    updateSrc "bgimg" "test/img/bg.png"
    updateTxt "dialog" "はじめからを選択しました"
    idle
    updateTxt "dialog" "もう一度スペースを押すと終了します"
    idle

load :: NovelGame ()
load = do
    loadComponents Body.simple
    updateTxt "dialog" "つづきからを選択しました"
    idle