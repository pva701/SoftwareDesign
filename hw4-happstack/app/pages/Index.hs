{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
module Index where

import           Happstack.Server                (Response, ServerPart, ok, setHeader,
                                                  toResponse)
import           Model
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet                     (shamlet)

index::[(TaskList, [Task])]->ServerPart Response
index sch = ok . setHeader "Content-Type" "text/html;charset=utf-8". toResponse . renderHtml $ [shamlet|
$doctype 5
<html lang="en" style="margin-bottom: 50px">
    <head>
        <meta charset="UTF-8" content="text/html"/>
        <title>Schedule

        <script src="js/jquery.min.js">
        <link href="css/bootstrap.css" rel="stylesheet"/>
        <script src="js/bootstrap.js">
        <link href="css/custom.css" rel="stylesheet"/>
        <script type="text/javascript" src="js/handlebars-v3.0.3.js">

    <body>
        <div id="schedule">
            $forall (TaskList tlid tlname, tasks) <- sch
                <div class="list-group col-md-4" id="taskList#{tlid}">
                    <div class="list-group-item active heading">
                        #{tlname}
                        <button type="button" class="close" data-dismiss="alert" aria-hidden="true"  onclick="delTaskList(this.parentElement.parentElement)">&times;

                    $forall (Task tid tname status) <- tasks
                        <div class="list-group-item list-group-item-action container" id="taskList#{tlid}_task#{tid}">
                            #{tname}
                            <button type="button" class="btn btn-danger float-md-right" onclick="delTask(this.parentElement)"><span class="glyphicon glyphicon-trash">
                            $case status
                                $of NotCompleted
                                    <button id="comp" type="button" class="btn btn-success float-md-right" onclick="completeTask(this.parentElement)"><span class="glyphicon glyphicon-check">
                                $of Completed
                                    <span class="tag tag-success float-md-right"><span class="glyphicon glyphicon-ok"></span>

                    <div class="list-group-item list-group-item-action form-inline" id="addTaskItem">
                        <input type="text" class="form-control" id="tName">
                        <button type="button" class="btn btn-success float-md-right" onclick="addTask(this.parentElement.parentElement)"><span class="glyphicon glyphicon-plus">


    <div class="tasklist-overlay">
        <div class="container">
            <div class="form-inline text-md-center" id="addTaskListItem">
                <div class="form-group">
                    <label>Add Task List
                    <input type="text" class="form-control" style="width: 300px" id="tlName"/>
                <button type="button" id="newTL" class="btn btn-success" onclick="addTaskList(this.parentElement)"><span class="glyphicon glyphicon-list-alt">

        <script id="task-template" type="text/x-handlebars-template">
            <div class="list-group-item list-group-item-action container" id="{{id}}">
                {{name}}
                <button type="button" class="btn btn-danger float-md-right" onclick="delTask(this.parentElement)"><span class="glyphicon glyphicon-trash">
                <button type="button" id="comp" class="btn btn-success float-md-right" onclick="completeTask(this.parentElement)"><span class="glyphicon glyphicon-check">

        <script id="taskList-template" type="text/x-handlebars-template">
            <div class="list-group col-md-4" id="{{id}}">
                <div class="list-group-item active heading">
                    {{name}}
                    <button type="button" class="close" data-dismiss="alert" aria-hidden="true"  onclick="delTaskList(this.parentElement.parentElement)">&times;

                <div class="list-group-item list-group-item-action form-inline" id="addTaskItem">
                    <input type="text" class="form-control" id="tName"/>
                    <button type="button" class="btn btn-success float-md-right" onclick="addTask(this.parentElement.parentElement)"><span class="glyphicon glyphicon-plus">

        <script>
            var taskTemplate = Handlebars.compile($('#task-template').html());
            var taskListTemplate = Handlebars.compile($('#taskList-template').html());

            function delTask(ths) {
                var id = ths.id;
                var pos = ths.id.indexOf('_');
                var taskList = ths.id.substring("taskList".length, pos);
                var task = ths.id.substring(pos + "task".length + 1);
                console.log(taskList + " " + task);
                \$.ajax({
                    type: "DELETE",
                    url: "/task/" + taskList + "/" + task,
                    success: function(msg) {
                        msg = JSON.parse(msg);
                        if (msg.success) {
                            \$('#' + id).remove();
                        }
                    }
                });
            }

            function delTaskList(ths) {
                var id = ths.id.substring("taskList".length);
                \$.ajax({
                    type: "DELETE",
                    url: "/taskList/" + id,
                    success: function(msg) {
                        msg = JSON.parse(msg);
                        console.log(msg);
                        if (msg.success) {
                            \$('#' + ths.id).remove();
                        }
                    }
                });
            }

            function addTask(ths) {
                var field = $('#' + ths.id + ' #tName');
                var name = field.val().trim();
                if (name == "") {
                    return;
                }
                var taskListId = ths.id.substring("taskList".length);
                \$.ajax({
                    type: "PUT",
                    url: "/task/" + taskListId + "/" + name,
                    success: function(msg) {
                        msg = JSON.parse(msg);
                        console.log(msg);
                        if (msg.success) {
                            var html = taskTemplate({name:name, id: 'taskList' + taskListId + '_task' + msg.id});
                            field.val(' ');
                            \$(html).insertBefore('#' + ths.id + ' #addTaskItem');
                        }
                    }
                });
            }

            function addTaskList(ths) {
                var field = $('#' + ths.id + ' #tlName');
                var name = field.val().trim();
                if (name == "") {
                    return;
                }
                \$.ajax({
                    type: "PUT",
                    url: "/taskList/" + name,
                    success: function(msg) {
                        msg = JSON.parse(msg);
                        if (msg.success) {
                            field.val('');
                            \$('#schedule').append(taskListTemplate({name: name, id: 'taksList' + msg.id}));
                        }
                    }
                });
            }

            function completeTask(ths) {
                var pos = ths.id.indexOf('_');
                var taskList = ths.id.substring("taskList".length, pos);
                var task = ths.id.substring(pos + "task".length + 1);
                \$.ajax({
                    type: "POST",
                    url: "/completeTask/" + taskList + "/" + task,
                    success: function(msg) {
                        msg = JSON.parse(msg);
                        console.log(msg);
                        if (msg.success) {
                            console.log(msg.success);
                            \$('#' + ths.id + ' #comp').replaceWith('<span class="tag tag-success float-md-right"><span class="glyphicon glyphicon-ok"></span></span>');
                        }
                    }
                });
            }


            \$("#tlName").keyup(function(event){
                if(event.keyCode == 13){
                    \$("#newTL").click();
                }
            });

|]
