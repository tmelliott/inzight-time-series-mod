module_name <- "TimeSeriesMod"

TimeSeriesMod <- setRefClass(
    "TimeSeriesMod",
    contains = "iNZModule",
    fields = list(
        ts_object = "ANY",
        available_vars = "character",
        time_var = "ANY",
        key_var = "ANY",
        key_filter = "ANY",
        key_hl = "ANY",
        measure_var = "ANY",
        g_subset = "ANY",
        g_hl = "ANY",
        all_plot_types = "ANY",
        plot_type = "ANY",
        vart = "ANY",
        key_o = "ANY",
        has_gaps = "ANY",
        g_smooth = "ANY",
        sm_toggle = "ANY",
        sm_tl = "ANY",
        sm_t = "ANY",
        timer = "ANY"
    ),
    methods = list(
        initialize = function(gui, ...) {
            callSuper(gui,
                help = "https://inzight.nz/user_guides/add_ons/?topic=time_series",
                ...
            )

            initFields(
                ts_object = NULL,
                available_vars = names(GUI$getActiveData()),
                timer = NULL
            )

            # some basic init settings:
            section_header_font <- list(
                weight = "bold"
            )

            var_tbl <- glayout()
            ii <- 1L

            ## --- specify the time column
            time_var <<- gcombobox(
                available_vars,
                selected = 1L,
                handler = function(h, ...) {
                    create_ts_object()
                }
            )
            var_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE, fill = TRUE] <- "Time :"
            var_tbl[ii, 2L, expand = TRUE] <- time_var
            ii <- ii + 1L

            ## --- specify the key column(s)
            key_var <<- gtable(
                list("Keys" = available_vars),
                multiple = TRUE
            )
            size(key_var) <<- c(-1, 110)
            addHandlerSelectionChanged(key_var,
                handler = function(h, ...) {
                    create_ts_object()
                }
            )
            var_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE, fill = TRUE] <- "Key :"
            var_tbl[ii, 2L, expand = TRUE] <- key_var
            ii <- ii + 1L

            ## put controls in a frame
            g_timeinfo <- gframe("Time information", horizontal = FALSE)
            g_timeinfo$set_borderwidth(5L)
            add(g_timeinfo, var_tbl)
            add_body(g_timeinfo)

            # --- specify the data column
            vartypes <- iNZightTools::vartypes(GUI$getActiveData()[available_vars[-1]])
            measure_var <<- gtable(
                data.frame(Variable = available_vars[-1][vartypes == "num"]),
                multiple = TRUE,
            )
            measure_var$set_index(1L)
            addHandlerSelectionChanged(measure_var,
                handler = function(h, ...) {
                    update_options()
                }
            )

            g_subset <<- gvbox()
            g_subset_frame <- gframe("Filter subset", horizontal = FALSE, container = g_subset)
            g_subset$set_borderwidth(5L)
            key_filter <<- gslider(container = g_subset, handler = function(h, ...) {
                update_options()
            })
            visible(g_subset) <<- FALSE

            g_vars <- gframe("Choose variables", horizontal = FALSE)
            g_vars$set_borderwidth(5L)
            size(measure_var) <<- c(-1, 160)
            vart <<- gradio(
                c("Numeric variables", "Categorical variables"), 1L, TRUE,
                handler = function(h, ...) {
                    create_ts_object()
                }
            )
            key_o <<- glabel(paste0(
                "! Too many key levels and/or variables selected,\n",
                "some functionalities might be affected."
            ))
            visible(key_o) <<- FALSE
            has_gaps <<- glabel(paste0(
                "! Gap(s) detected in some/all variable(s) selected,\n",
                "smoothers will be disabled for those variable(s)."
            ))
            visible(has_gaps) <<- FALSE
            add(g_vars, vart)
            add(g_vars, measure_var, expand = TRUE)
            add(g_vars, key_o, anchor = c(-1, 1))
            add(g_vars, has_gaps, anchor = c(-1, 1))

            g_hl <<- gvbox()
            g_hl_frame <- gframe("Highlight subset", horizontal = FALSE, container = g_hl)
            g_hl$set_borderwidth(5L)
            key_hl <<- gslider(container = g_hl, handler = function(h, ...) {
                update_options()
            })
            visible(g_hl) <<- FALSE

            g_plottype <- gframe("Plot type", horizontal = TRUE)
            g_plottype$set_borderwidth(5L)
            all_plot_types <<- c("Default", "Seasonal", "Decomposition", "Forecast")
            plot_type <<- gradio(
                all_plot_types,
                horizontal = TRUE,
                container = g_plottype,
                handler = function(h, ...) {
                    update_options()
                }
            )

            g_smooth <<- gvbox()
            g_smooth_frame <- gframe("Smoother settings", horizontal = FALSE, container = g_smooth)
            g_smooth$set_borderwidth(5L)
            sm_toggle <<- gcheckbox(
                "Enable smoother", TRUE,
                container = g_smooth,
                handler = function(h, ...) {
                    update_options()
                }
            )
            sm_tl <<- glabel("Smoothing parameter:", container = g_smooth, anchor = c(-1, 1))
            sm_t <<- gslider(container = g_smooth, handler = function(h, ...) {
                if (!is.null(timer) && timer$started) timer$stop_timer()
                timer <<- gtimer(200, function(...) update_plot(), one.shot = TRUE)
            })

            add_body(g_subset)
            add_body(g_vars)
            add_body(g_plottype)
            add_body(g_hl)
            add_body(g_smooth)

            guess_key()
        },
        guess_key = function() {
            d <- GUI$getActiveData()
            cols <- names(d)
            cat_cols <- cols[!sapply(d, is.numeric)][-time_var$get_index()]
            t_var <- names(d)[[time_var$get_index()]]
            maybe_key <- NULL
            ts_test <- try(
                iNZightTS2::inzightts(d, index = t_var, key = maybe_key),
                silent = TRUE
            )
            if (!inherits(ts_test, "inz_ts")) {
                for (key_cand in cat_cols) {
                    maybe_key <- c(maybe_key, key_cand)
                    ts_test <- try(
                        iNZightTS2::inzightts(GUI$getActiveData(), index = t_var, key = maybe_key),
                        silent = TRUE
                    )
                    if (inherits(ts_test, "inz_ts")) break
                }
            }
            if (length(maybe_key)) {
                key_msg <- paste0(
                    ifelse(length(maybe_key) > 1, "c(", ""),
                    paste(maybe_key, collapse = ", "),
                    ifelse(length(maybe_key) > 1, ")", "")
                )
                message("Guessing key = ", key_msg)
                key_var$set_value(maybe_key)
            }

            create_ts_object()
        },
        create_ts_object = function() {
            ri <- ti <- time_var$get_index()
            key_col <- svalue(key_var)
            if (length(key_col)) {
                ki <- which(available_vars %in% key_col)
                ri <- c(ri, ki)
            } else {
                ki <- NULL
                key_col <- NULL
            }
            t <- try(
                iNZightTS2::inzightts(GUI$getActiveData(),
                    index = ti,
                    key = ki
                ),
                silent = TRUE
            )
            if (inherits(t, "try-error")) {
                gmessage("Unable to create temporal object. Maybe you forgot to specify the keys?")
                print(t)
                ts_object <<- NULL
            } else {
                ts_object <<- t
            }

            measure_val <- svalue(measure_var)
            vartypes <- iNZightTools::vartypes(GUI$getActiveData()[available_vars[-ri]])
            vartc <- ifelse(svalue(vart) == "Numeric variables", "num", "cat")
            mvarc <- available_vars[-ri][vartypes == vartc]
            measure_var$set_items(data.frame(Variable = mvarc))
            if (any(measure_val %in% mvarc)) {
                measure_var$set_value(measure_val[measure_val %in% mvarc])
            } else {
                measure_var$set_index(1L)
            }

            update_options()
        },
        update_options = function() {
            visible(key_o) <<- tsibble::n_keys(ts_object) * length(svalue(measure_var)) > 10
            visible(has_gaps) <<- any(is.na(ts_object[svalue(measure_var)]))
            if (!length(svalue(key_var))) {
                visible(g_subset) <<- FALSE
            } else {
                show_all <- svalue(plot_type) != "Decomposition" &&
                    svalue(vart) == "Numeric variables" &&
                    (!visible(key_o) || svalue(plot_type) != "Forecast")
                (key_info <- ts_object |>
                    tsibble::key_data() |>
                    dplyr::select(-.rows) |>
                    apply(1, \(x) paste(x, collapse = "/")) |>
                    as.character()) |>
                    (\(x) (if (show_all) c("(Show all)", x) else x))() |>
                    (\(x) factor(x, x))() |>
                    key_filter$set_items()
                visible(g_subset) <<- TRUE
            }
            if (!length(svalue(key_var)) || svalue(key_filter) != "(Show all)" ||
                svalue(plot_type) != "Default") {
                visible(g_hl) <<- FALSE
            } else {
                c("(Show all)", key_info) |>
                    (\(x) factor(x, x))() |>
                    key_hl$set_items()
                visible(g_hl) <<- TRUE
            }
            opt_aval <- list(
                ## If c(sm_toggle, sm_tl, sm_t, g_smooth) are visible
                Default = c(TRUE, TRUE, TRUE, TRUE),
                Seasonal = c(FALSE, TRUE, TRUE, TRUE),
                Decomposition = c(FALSE, TRUE, TRUE, TRUE),
                Forecast = c(FALSE, FALSE, FALSE, FALSE)
            )
            (\(opt, is_visible) opt$set_visible(is_visible)) |>
                mapply(
                    opt = list(sm_toggle, sm_tl, sm_t, g_smooth),
                    is_visible = as.list(opt_aval[[svalue(plot_type)]])
                )
            visible(g_smooth) <<- visible(g_smooth) && vart$get_index() == 1L
            if (svalue(plot_type) == "Default") {
                visible(sm_tl) <<- visible(g_smooth) && visible(sm_tl) && svalue(sm_toggle)
                visible(sm_t) <<- visible(g_smooth) && visible(sm_t) && svalue(sm_toggle)
            }
            plot_aval <- all_plot_types
            if (svalue(vart) == "Categorical variables") {
                if (length(svalue(measure_var)) > 1) {
                    gmessage("Please select one variable at a time for plotting categorical variable.")
                    measure_var$set_value(svalue(measure_var)[1L])
                }
                plot_aval <- plot_aval[1]
            } else if (length(svalue(measure_var)) > 1) {
                plot_aval <- plot_aval[-3]
            }
            if (length(plot_aval) != length(plot_type$get_items()) ||
                !all(plot_aval == plot_type$get_items())) {
                plot_type$set_items(plot_aval)
            }

            update_plot()
        },
        update_plot = function() {
            mvar <- svalue(measure_var)
            if (is.null(ts_object) || !length(mvar)) {
                return()
            }
            ts_p <- ts_object
            if (svalue(key_filter) != "(Show all)") {
                key_i <- key_filter$get_index() - 1L + (svalue(plot_type) == "Decomposition")
                ts_p <- tsibble::key_data(ts_p)[key_i, ] |>
                    dplyr::left_join(ts_p, by = tsibble::key_vars(ts_p), multiple = "all") |>
                    tsibble::as_tsibble(index = !!tsibble::index(ts_p), key = NULL) |>
                    inzightts()
            }
            key_to_hl <- NULL
            if (length(svalue(key_var)) && key_hl$get_index() != 1L) {
                key_to_hl <- key_hl$get_index() - 1L
            }

            dev.hold()
            on.exit(dev.flush(dev.flush()))

            print(switch(which(svalue(plot_type) == all_plot_types),
                # default
                plot(ts_p, var = mvar, emphasise = key_to_hl, smoother = svalue(sm_toggle), t = svalue(sm_t)),
                # seasonal
                seasonplot(ts_p, var = mvar, t = svalue(sm_t)),
                # decomposition
                plot(decomp(ts_p, var = mvar, t = svalue(sm_t))),
                # forecast
                plot(predict(ts_p, var = mvar))
            ))
        }
    )
)

OldTimeSeriesModule <- setRefClass(
    "OldTimeSeriesModule",
    contains = "iNZModule",
    fields = list(
        activeData  = "data.frame",
        timeVarType = "ANY",
        timeVar     = "ANY",
        timePeriodList = "ANY",
        timeFreqList = "ANY", timeFreqNum = "ANY",
        timeStartPeriod = "ANY", timeStartSeason = "ANY",
        timePeriod = "ANY", timeFreq = "ANY", timeStart = "ANY",
        patternType = "numeric",
        smootherChk = "ANY", show.smoother = "logical",
        smthSlider  = "ANY", smoothness = "numeric",
        tsObj       = "ANY",
        yLab        = "ANY", xLab = "ANY",
        xlimLower   = "ANY", xlimUpper   = "ANY",
        modLimEqual = "ANY", modLimLower = "ANY", modLimUpper = "ANY",
        plotType = "ANY", plottype = "numeric",
        compareChk = "ANY", compare = "numeric",
        animateBtn  = "ANY", pauseBtn = "ANY",
        recomposeBtn = "ANY", recomposeResBtn = "ANY", decomp = "ANY",
        recompProg = "ANY",
        forecastBtn = "ANY", forecasts   = "ANY",
        forecastError = "ANY",
        timer = "ANY", playTimer = "ANY",
        timeVarSelect = "ANY",
        varSelect = "ANY"
    ),
    methods = list(
        initialize = function(gui, ...) {
            if (packageVersion('iNZightTS') > numeric_version('1.5.8')) {
                gmessage("You have a newer version of iNZightTS installed. Please update this module.")
                return(NULL)
            }
            callSuper(gui,
                help = "https://inzight.nz/user_guides/add_ons/?topic=time_series",
                ...
            )

            initFields(
                patternType = 1,
                show.smoother = TRUE,
                smoothness = 15,
                tsObj = NULL,
                plottype = 1,
                compare = 1,
                timeFreq = NA,
                timeStart = c(1, 1),
                timePeriod = NULL,
                recompProg = c(0, 0),
                timer = NULL
            )

            dat = GUI$getActiveData()
            activeData <<- tsData(dat)
            timeVar <<- getTime(activeData, index = FALSE)

            ## playBtn <- iNZight:::gimagebutton(stock.id = "media-play",
            #       handler = function(h, ...) updatePlot(animate = TRUE))
            # TODO: move this to a 'toolbar.R' file
            GUI$plotToolbar$update("export", refresh = "updatePlot")
                #, extra = list(playBtn))

            ################
            ###  fields  ###
            ################
            frameFont = list(weight = "bold")

            #################################
            ###  set up frame containers  ###
            #################################
            g1 = gframe("Time Information", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            g2 = gframe("Model Settings", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            # addSpring(mainGrp)

            midGrp <- ggroup(container = mainGrp, fill = TRUE)
            g3 = gframe("Series Variables", pos = 0.5, horizontal = FALSE,
                        container = midGrp, fill = TRUE)
            g5 = gframe("Plot Type Options", pos = 0.5, horizontal = FALSE,
                        container = midGrp, fill = TRUE, expand = TRUE)

            g4 = gexpandgroup("Customize Labels",
                # pos = 0.5,
                horizontal = FALSE,
                container = mainGrp
            )

            g6 = gexpandgroup("Adjust limits",
                horizontal = FALSE,
                container = mainGrp
            )


            g1$set_borderwidth(8)
            g2$set_borderwidth(8)
            g3$set_borderwidth(8)
            g4$set_borderwidth(8)

            g5$set_borderwidth(8)
            g6$set_borderwidth(8)

            ## bold-faced title for the frames
            frames = getToolkitWidget(mainGrp)$getChildren()
            mainGrp$set_rgtk2_font(frames[[1]]$getChildren()[[2]], frameFont)
            mainGrp$set_rgtk2_font(frames[[2]]$getChildren()[[2]], frameFont)
            midGrp$set_rgtk2_font(
                getToolkitWidget(midGrp)$getChildren()[[1]]$getChildren()[[2]],
                frameFont
            )
            midGrp$set_rgtk2_font(
                getToolkitWidget(midGrp)$getChildren()[[2]]$getChildren()[[2]],
                frameFont
            )
            mainGrp$set_rgtk2_font(
                frames[[4]]$getChildren()[[2]],
                frameFont
            )
            mainGrp$set_rgtk2_font(
                frames[[5]]$getChildren()[[2]],
                frameFont
            )

            ############
            ###  g1  ###
            ############
            ## FOR MAIN LAYOUT
            g1_layout = glayout(container = g1)
            timeVarType <<- gradio(
                c("Select time variable", "Provide time manually"),
                selected = 1,
                horizontal = FALSE
            )
            g1_layout[1, 1:2, expand = TRUE] = timeVarType

            ## FOR LAYOUT A
            g1a_layout = glayout(container = g1)
            ## g1a options

            timeVarSelect <<- gcombobox(names(activeData),
                selected = match(timeVar, names(activeData), nomatch = 0),
                handler = function(h, ...) {
                    timeVar <<- svalue(h$obj)
                    updatePlot()
                }
            )
            ## g1a labels
            g1a_lab1   = glabel("Select time variable:")
            ## g1a layout
            g1a_layout[2, 1, expand = TRUE, anchor = c(-1, 0)] = g1a_lab1
            g1a_layout[2, 2, expand = TRUE]   = timeVarSelect

            ## FOR LAYOUT B
            g1b_layout = glayout(container = g1, spacing = 2)
            visible(g1b_layout) = FALSE

            ## g1b options
            ii <- 1

            lbl <- glabel("Period :")
            timePeriodList <<- gcombobox(c("Year", "Week", "Day"),
                selected = 0,
                handler = function(h, ...) {
                    timePeriod <<- svalue(h$obj)
                    blockHandlers(varSelect)
                    timeFreqList$set_items(
                        c(names(freqOpts[[svalue(h$obj)]]), "Custom")
                    )
                    unblockHandlers(varSelect)
                    svalue(startlbl1) <- "Year"
                    varSelect$invoke_change_handler()
                }
            )
            g1b_layout[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            g1b_layout[ii, 2, expand = TRUE, fill = TRUE] <- timePeriodList
            ii <- ii + 1

            lbl <- glabel("Frequency* :")
            freqOpts <- list(
                "Year" = c(
                    "Yearly (1)" = 1,
                    "Quarterly (4)" = 4,
                    "Monthly (12)" = 12,
                    "Weekly (52)" = 52,
                    "Daily (365/366)" = 365.25
                ),
                "Week" = c(
                    "Daily (7)" = 7,
                    "Daily - work week (5)" = 5
                ),
                "Day"  = c(
                    "Hourly (24)" = 24
                )
            )
            timeFreqList <<- gcombobox(character(),
                selected = 0,
                handler = function(h, ...) {
                    blockHandlers(varSelect)
                    if (svalue(h$obj) == "Custom") {
                        enabled(timeFreqNum) <<- TRUE
                    } else {
                        enabled(timeFreqNum) <<- FALSE
                        svalue(timeFreqNum) <<-
                            freqOpts[[timePeriod]][svalue(h$obj)]
                    }
                    timeFreqNum$invoke_change_handler()
                    unblockHandlers(varSelect)
                    season.name <- svalue(h$obj)
                    if (season.name == "Custom") {
                        season.name <- "Season"
                    } else {
                        season.name <- gsub("ly$", "",
                            strsplit(season.name, " ")[[1]][1])
                        if (season.name == "Dai") season.name <- "Day"
                    }
                    svalue(startlbl2) <- season.name
                    varSelect$invoke_change_handler()
                }
            )
            timeFreqNum <<- gspinbutton(1, 1000, by = 1,
                value = 1,
                handler = function(h, ...) {
                    timeFreq <<- svalue(h$obj)
                    blockHandlers(varSelect)
                    svalue(timeStartSeason) <<-
                        min(svalue(timeStartSeason), timeFreq)
                    if (svalue(h$obj) == 1) {
                        enabled(timeStartSeason) <<- FALSE
                        visible(startlbl2) <- FALSE
                    } else {
                        enabled(timeStartSeason) <<- TRUE
                        visible(startlbl2) <- TRUE
                    }
                    unblockHandlers(varSelect)
                    varSelect$invoke_change_handler()
                }
            )
            g1b_layout[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            g1b_layout[ii, 2, expand = TRUE, fill = TRUE] <- timeFreqList
            g1b_layout[ii, 3, expand = TRUE, fill = TRUE] <- timeFreqNum
            ii <- ii + 1

            lbl <- glabel("*How many observations per period?")
            font(lbl) <- list(size = 9)
            g1b_layout[ii, 2:3, anchor = c(-1, 1), expand = TRUE] <- lbl
            ii <- ii + 1

            ii <- ii + 1

            lbl <- glabel("Start date : ")
            timeStartPeriod <<- gspinbutton(0, 1e5, by = 1, value = 1,
                handler = function(h, ...) {
                    timeStart <<- c(svalue(h$obj), svalue(timeStartSeason))
                    varSelect$invoke_change_handler()
                })
            timeStartSeason <<- gspinbutton(0, 1e5, by = 1, value = 1,
                handler = function(h, ...) {
                    if (svalue(h$obj) > timeFreq)
                        svalue(h$obj) <- timeFreq
                    timeStart <<- c(svalue(timeStartPeriod), svalue(h$obj))
                    varSelect$invoke_change_handler()
                })
            g1b_layout[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            g1b_layout[ii, 2, expand = TRUE, fill = TRUE] <- timeStartPeriod
            g1b_layout[ii, 3, expand = TRUE, fill = TRUE] <- timeStartSeason
            ii <- ii + 1

            startlbl1 <- glabel("Period")
            font(startlbl1) <- list(size = 9)
            startlbl2 <- glabel("Season")
            font(startlbl2) <- list(size = 9)
            g1b_layout[ii, 2, anchor = c(-1, 1), expand = TRUE] <- startlbl1
            g1b_layout[ii, 3, anchor = c(-1, 1), expand = TRUE] <- startlbl2
            ii <- ii + 1

            addHandlerChanged(timeVarType, handler = function(h,...) {
                if (svalue(h$obj, index = TRUE) == 1) {
                    visible(g1a_layout) = TRUE
                    visible(g1b_layout) = FALSE
                } else {
                    visible(g1a_layout) = FALSE
                    visible(g1b_layout) = TRUE
                }
                varSelect$invoke_change_handler()
            })

            ############
            ###  g2  ###
            ############
            g2_layout = glayout(container = g2, spacing = 5)
            g2_opt1   = gradio(c("Multiplicative", "Additive"),
                selected = patternType,
                horizontal = TRUE,
                handler = function(h, ...) {
                    patternType <<- svalue(h$obj, index = TRUE)
                    updatePlot()
                }
            )

            g2_layout[1, 1, anchor = c(1, 0), expand = TRUE] <-
                glabel("Seasonal pattern :")
            g2_layout[1, 2, expand = TRUE] = g2_opt1

            ## Smoother
            smthSlider <<- gslider(0, 100, by = 0.1,
                value = smoothness,
                handler = function(h, ...) {
                    smoothness <<- svalue(h$obj)
                    if (!is.null(timer))
                        if (timer$started)
                            timer$stop_timer()

                    timer <<- gtimer(200, function(...) updatePlot(),
                        one.shot = TRUE
                    )
                }
            )

            g2_layout[2, 1, anchor = c(1, 0), expand = TRUE] <-
                glabel("Smoothness :")
            g2_layout[2, 2, fill = TRUE, expand = TRUE] <- smthSlider

            ## Checkbox to hide/show smoother
            smootherChk <<- gcheckbox("Show smoother",
                checked = show.smoother,
                handler = function(h, ...) {
                    show.smoother <<- svalue(h$obj)
                    enabled(smthSlider) <<- show.smoother
                    updatePlot()
                }
            )
            g2_layout[3, 2, fill = TRUE, expand = TRUE] <- smootherChk

            ############
            ###  g3  ###
            ############
            ## NOTE:
            ##   need to change the variable selection widget for when there
            ##   are many variables which will expand the widget.
            g3_layout = glayout(container = g3)
            varSelect <<- gtable(
                names(activeData)[! names(activeData) %in% timeVar],
                multiple = TRUE
            )
            size(varSelect) <<- c(floor(size(GUI$leftMain)[1] * 0.5), 200)
            g3_layout[1, 1, anchor = c(-1, 0), expand = TRUE] <-
                glabel("Hold CTRL to select many")
            g3_layout[2, 1, expand = TRUE] = varSelect



            addHandlerSelectionChanged(varSelect, function(h, ...) {
                if (length(svalue(varSelect)) == 0) {
                    visible(novar) <- TRUE
                    return()
                }
                visible(novar) <- FALSE

                ## make dataset an iNZightTS object
                var_ind <- which(names(activeData) %in% svalue(h$obj))
                if (length(var_ind) == 1) {
                    visible(onevar) <- TRUE
                    visible(multivar) <- FALSE
                } else {
                    visible(onevar) <- FALSE
                    visible(multivar) <- TRUE
                }
                can_multiply <- all(sapply(var_ind, function(i) all(activeData[[i]] > 0)))
                enabled(g2_opt1) <- can_multiply
                if (!can_multiply) svalue(g2_opt1, index = TRUE) <- 2

                if ((svalue(timeVarType, TRUE) == 1 && !is.na(timeVar)) ||
                    (svalue(timeVarType, TRUE) == 2 && !is.null(timePeriod) && !is.na(timeFreq)) ) {
                    # tryCatch({
                        if (svalue(timeVarType, TRUE) == 1) {
                            tso <- iNZightTS::iNZightTS(
                                data = activeData,
                                var = var_ind,
                                time.col =
                                    which(colnames(activeData) == timeVar)
                            )
                        } else {
                            tso <- iNZightTS::iNZightTS(
                                data = activeData,
                                var = var_ind,
                                start = timeStart,
                                freq = timeFreq
                            )
                        }
                        tsObj <<- tso
                        updatePlot()
                    # },
                    # error = function(e) {
                    #     gmessage(
                    #         paste(sep="\n\n",
                    #             "Error creating Time Series object",
                    #             e$message
                    #         ),
                    #         title = "Error creating time series",
                    #         icon = "error",
                    #         parent = GUI$win
                    #     )
                    # },
                    # finally = {})

                    # if freq=1, disable seasonal/forecast/single-graph
                    if (tsObj$freq == 1) {
                        plotType$set_items(c("Standard", "Decomposition"))
                        compareChk$set_items("Separate graphs")
                    } else {
                        plotType$set_items(c("Standard", "Decomposition", "Seasonal", "Forecast"))
                        compareChk$set_items(c("Single graph", "Separate graphs"))
                    }

                } else {
                    # Something more helpful
                    tsObj <<- NULL

                }

            })

            addHandlerChanged(timeVarSelect, function(h, ...) {
                varSelect$set_items(
                    names(activeData)[! names(activeData) %in% timeVar]
                )
            })


            ############
            ###  g5  ###
            ############

            onevar <- gvbox(container = g5)
            addSpring(onevar)
            plotType <<- gradio(
                c("Standard", "Decomposition", "Seasonal", "Forecast"),
                selected = plottype,
                container = onevar,
                expand = TRUE,
                handler = function(h, ...) {
                    plottype <<- svalue(h$obj, index = TRUE)
                    visible(animateBtn) <<- svalue(h$obj, TRUE) == 1
                    visible(pauseBtn) <<- svalue(h$obj, TRUE) == 1
                    visible(recomposeBtn) <<- FALSE
                    visible(recomposeResBtn) <<- FALSE
                    visible(forecastBtn) <<- FALSE
                    updatePlot()
                }
            )

            tsenv <- new.env()
            assign("stopAnimation", FALSE, envir = tsenv)
            runAnimation <- gaction("Animate",
                icon = "gtk-media-play",
                handler = function(h, ...) {
                    assign("stopAnimation", FALSE, envir = tsenv)
                    enabled(animateBtn) <<- FALSE
                    enabled(pauseBtn) <<- TRUE
                    iNZightTS::rawplot(tsObj,
                        multiplicative = (patternType == 1),
                        ylab = svalue(yLab),
                        xlab = svalue(xLab),
                        animate = TRUE,
                        t = smoothness,
                        e = tsenv
                    )
                    enabled(pauseBtn) <<- FALSE
                    enabled(animateBtn) <<- TRUE
                }
            )
            pauseAnimation <- gaction("End Animation",
                icon = "gtk-media-stop",
                handler = function(h, ...) {
                    assign("stopAnimation", TRUE, envir = tsenv)
                }
            )

            animateBtn <<- gbutton(action = runAnimation, container = onevar)
            pauseBtn <<- gbutton(action = pauseAnimation, container = onevar)
            enabled(pauseBtn) <<- FALSE

            playTimer <<- NULL
            recomposeBtn <<- gbutton("Recompose",
                container = onevar,
                handler = function(h, ...) {
                    ## this button is _ if _
                    # - Recompose | is.null(playTimer)
                    # - Pause | !is.null(playTimer)
                    blockHandlers(recomposeBtn)
                    blockHandlers(recomposeResBtn)
                    on.exit(unblockHandlers(recomposeBtn))
                    on.exit(unblockHandlers(recomposeResBtn), add = TRUE)
                    if (is.null(playTimer) || !playTimer$started) {
                        if (all(recompProg == c(1, nrow(activeData)))) {
                            recompProg <<- c(0, 0)
                            updatePlot()
                            svalue(recomposeResBtn) <<- "Recompose result"
                        }
                        svalue(recomposeBtn) <<- "Pause"
                        playTimer <<- gtimer(10,
                            function(data) {
                                if (recompProg[2] >= nrow(activeData)) {
                                    if (recompProg[1] == 0)
                                        recompProg <<- c(1, 0)
                                    else {
                                        playTimer$stop_timer()
                                        blockHandlers(recomposeBtn)
                                        blockHandlers(recomposeResBtn)
                                        on.exit(unblockHandlers(recomposeBtn))
                                        on.exit(unblockHandlers(recomposeResBtn), add = TRUE)
                                        svalue(recomposeBtn) <<- "Replay"
                                        svalue(recomposeResBtn) <<- "Reset"
                                        return()
                                    }
                                } else {
                                    recompProg[2] <<- recompProg[2] + 1
                                }
                                updatePlot()
                            }
                        )
                    } else {
                        playTimer$stop_timer()
                        svalue(recomposeBtn) <<- "Recompose"
                    }

                }
            )
            visible(recomposeBtn) <<- FALSE
            recomposeResBtn <<- gbutton("Recompose Result", container = onevar)
            addHandlerClicked(recomposeResBtn,
                handler = function(h, ...) {
                    assign("stopAnimation", TRUE, envir = tsenv)
                    blockHandlers(h$obj)
                    on.exit(unblockHandlers(h$obj))
                    if (!is.null(playTimer))
                        if (playTimer$started) playTimer$stop_timer()
                    if (svalue(h$obj) == "Reset") {
                        recompProg <<- c(0, 0)
                        updatePlot()
                        svalue(recomposeResBtn) <<- "Recompose Result"
                    } else {
                        recompProg <<- c(1, nrow(activeData))
                        updatePlot()
                        svalue(recomposeResBtn) <<- "Reset"
                    }
                    blockHandlers(recomposeBtn)
                    on.exit(unblockHandlers(recomposeBtn), add = TRUE)
                    svalue(recomposeBtn) <<- "Recompose"
                }
            )
            visible(recomposeResBtn) <<- FALSE

            forecastBtn <<- gbutton("Forecasted Values",
                container = onevar,
                handler = function(h, ...) {
                    w <- gwindow("Time Series Forecasts", parent = GUI$win,
                                 width = 400, height = 300)
                    g <- gvbox(container = w)
                    t <- gtext(text = "",
                        container = g,
                        expand = TRUE,
                        wrap = FALSE,
                        font.attr = list(family = "monospace")
                    )
                    insert(t, capture.output(print(forecasts)))
                }
            )
            visible(forecastBtn) <<- FALSE
            forecastError <<- ggroup(container = onevar)
            glabel("Error fitting model ",
                container = forecastError)
            visible(forecastError) <<- FALSE
            iNZight:::gimagebutton(stock.id = "info",
                container = forecastError,
                handler = function(h, ...) {
                    gmessage(
                        paste(
                            "Sometimes the algorithm used (Holt Winters)",
                            "is unable to converge. This can be sensitive to",
                            "values in the data set. If you haven't already,",
                            "try unchecking the 'Use above limits' box under",
                            "'Adjust Limits', and then move the 'Fit model to data from'",
                            "sliders, which may help convergence."
                        ),
                        parent = GUI$win
                    )
                }
            )

            multivar <- ggroup(container = g5)
            compareChk <<- gradio(c("Single graph", "Separate graphs"),
                checked = compare,
                container = multivar,
                handler = function(h, ...) {
                    compare <<- svalue(h$obj, index = TRUE)
                    updatePlot()
                }
            )

            visible(onevar) <- FALSE
            visible(multivar) <- FALSE

            novar <- gvbox(container = g5)
            glabel("Select a Variable.", container = novar)
            lb <- glabel("(Hold CTRL to select multiple)", container = novar)
            font(lb) <- list(size = 8)



            ############
            ###  g4  ###
            ############
            g4_layout = glayout(container = g4)
            g4_lab1   = glabel("x-axis")
            g4_lab2   = glabel("y-axis")

            xLab <<- gedit(ifelse(!is.na(timeVar), timeVar, ""))
            yLab <<- gedit("")

            addHandlerKeystroke(xLab,
                handler = function(h, ...) {
                    if (!is.null(timer))
                        if (timer$started) timer$stop_timer()
                    timer <<- gtimer(200, function(...) {
                        updatePlot()
                    }, one.shot = TRUE)
                }
            )
            addHandlerKeystroke(yLab,
                handler = function(h, ...) {
                    if (!is.null(timer))
                        if (timer$started) timer$stop_timer()
                    timer <<- gtimer(200, function(...) {
                        updatePlot()
                    }, one.shot = TRUE)
                }
            )

            #size(xLab) <<- c(150, 21)
            #size(yLab) <<- c(150, 21)

            g4_layout[1, 1:2, expand = TRUE, anchor = c(-1, 0)] = g4_lab1
            g4_layout[2, 1:2, expand = TRUE, anchor = c(-1, 0)] = g4_lab2
            g4_layout[1, 3, expand = TRUE] = xLab
            g4_layout[2, 3, expand = TRUE] = yLab

            clearXlab <- iNZight:::gimagebutton(stock.id = "reset",
                handler = function(h, ...) {
                    svalue(xLab) <<- timeVar
                }
            )
            g4_layout[1, 4] <- clearXlab
            clearYlab <- iNZight:::gimagebutton(stock.id = "reset",
                handler = function(h, ...) {
                    svalue(yLab) <<- ""
                }
            )
            g4_layout[2, 4] <- clearYlab


            ############
            ###  g6  ###
            ############
            g6_layout = glayout(container = g6, homogeneous = TRUE)
            ii <- 1

            ## Control axis limits
            g6_layout[ii, 1, anchor = c(-1, 0), expand = TRUE] <-
                glabel("Display data from ... ")
            g6_layout[ii, 2, anchor = c(-1, 0), expand = TRUE] <-
                glabel("until ... ")
            ii <- ii + 1


            xlimLower <<- gslider(
                handler = function(h, ...) {
                    if (!is.null(timer))
                        if (timer$started)
                            timer$stop_timer()

                    timer <<- gtimer(200, function(...) updateLimits(),
                        one.shot = TRUE
                    )
                }
            )
            xlimUpper <<- gslider(
                handler = function(h, ...) {
                    if (!is.null(timer))
                        if (timer$started)
                            timer$stop_timer()

                    timer <<- gtimer(200, function(...) updateLimits(),
                        one.shot = TRUE
                    )
                }
            )
            g6_layout[ii, 1, expand = TRUE] <- xlimLower
            g6_layout[ii, 2, expand = TRUE] <- xlimUpper
            ii <- ii + 1

            updateLimits()

            ## Model limits
            modLimEqual <<- gcheckbox("Use above limits for fitting model",
                checked = TRUE)
            g6_layout[ii, 1:2, expand = TRUE] <- modLimEqual
            ii <- ii + 1

            modLimLower <<- gslider(
                handler = function(h, ...) {
                    if (!is.null(timer))
                        if (timer$started)
                            timer$stop_timer()

                    timer <<- gtimer(200, function(...) updateModLimits(),
                        one.shot = TRUE
                    )
                }
            )
            modLimUpper <<- gslider(
                handler = function(h, ...) {
                    if (!is.null(timer))
                        if (timer$started)
                            timer$stop_timer()

                    timer <<- gtimer(200, function(...) updateModLimits(),
                        one.shot = TRUE
                    )
                }
            )

            modlbl1 <- glabel("Fit model to data from ... ")
            modlbl2 <- glabel("until ... ")
            visible(modlbl1) <- visible(modlbl2) <- FALSE

            g6_layout[ii, 1, anchor = c(-1, 0), expand = TRUE] <- modlbl1
            g6_layout[ii, 2, anchor = c(-1, 0), expand = TRUE] <- modlbl2
            ii <- ii + 1

            g6_layout[ii, 1, expand = TRUE] <- modLimLower
            g6_layout[ii, 2, expand = TRUE] <- modLimUpper
            ii <- ii + 1

            updateModLimits()
            addHandlerChanged(modLimEqual,
                handler = function(h, ...) {
                    visible(modlbl1) <- visible(modlbl2) <- !svalue(h$obj)
                    updatePlot()
                }
            )

            ## IF time series variable is chosen, plot first variable.
            svalue(varSelect, index = TRUE) <<- 1
        },

        # ========
        # METHODS
        # ========
        ## returns the time variable index
        getTime = function(data, index = TRUE) {
            ## look for time or date
            ind <- sapply(names(data),
                function(x) {
                    t <- try(iNZightTS:::get.ts.structure(data[[x]]), silent = TRUE)
                    if (inherits(t, "try-error")) return(FALSE)
                    return(!identical(t, list(start = NA, frequency = NA)))
                }
            )
            if (any(ind)) {
                ind <- which(ind)[1]
            } else {
                time_re <- "([Tt][Ii][Mm][Ee])|([Dd][Aa][Tt][Ee])"
                ind <- grep(time_re, names(data))
                ind <- if (length(ind) == 0) 1 else ind[1]
            }
            if (index) return(ind)
            return(names(data)[ind])
        },

        ## checks for a time variable in dataset
        isTS = function(data) {
            return(length(getTime(data)) != 0)
        },

        ## drops categorical variables (except the time variable)
        tsData = function(data) {
            time_index = getTime(data)
            num_index = sapply(data, is.numeric)
            num_index[time_index] <- TRUE
            data[, num_index]
        },

        ## update limit sliders
        updateLimits = function(react = TRUE) {
            if (is.null(tsObj)) {
                visible(xlimLower) <<- visible(xlimUpper) <<- FALSE
                return()
            }

            # store old values
            xr <- range(time(tsObj$tsObj))
            xby <- 1 / tsObj$freq
            xx <- seq(xr[1], xr[2], by = xby)
            xd <- as.character(tsObj$data[[timeVar]])

            xlim <- xr
            if (svalue(xlimLower) > 0)
                xlim[1] <- xx[xd == svalue(xlimLower)]
            if (svalue(xlimUpper) > 0)
                xlim[2] <- xx[xd == svalue(xlimUpper)]

            ## if upper limit gets too low, disable lower slider
            if (xlim[2] <= min(xx) + 2) {
                enabled(xlimLower) <<- FALSE
            } else {
                enabled(xlimLower) <<- TRUE
                blockHandlers(xlimLower)
                xlimLower$set_items(xd[xx <= xlim[2] - 2])
                xlimLower$set_value(xd[xx == xlim[1]])
                unblockHandlers(xlimLower)
            }

            ## if lower limit gets too high, disable upper slider
            if (xlim[1] >= max(xx) - 2) {
                enabled(xlimUpper) <<- FALSE
            } else {
                enabled(xlimUpper) <<- TRUE
                blockHandlers(xlimUpper)
                xlimUpper$set_items(xd[xx >= xlim[1] + 2])
                xlimUpper$set_value(xd[xx == xlim[2]])
                unblockHandlers(xlimUpper)
            }

            visible(xlimLower) <<- visible(xlimUpper) <<- TRUE
            # don't want to react when being called by updatePlot!
            if (react) updatePlot()
        },

        updateModLimits = function(react = TRUE) {
            if (is.null(tsObj)) {
                visible(modLimLower) <<- visible(modLimUpper) <<- FALSE
                return()
            }
            if (svalue(modLimEqual)) {
                svalue(modLimLower) <<- svalue(xlimLower)
                svalue(modLimUpper) <<- svalue(xlimUpper)
                visible(modLimLower) <<- visible(modLimUpper) <<- FALSE
                return()
            }

            # store old values
            xr <- range(time(tsObj$tsObj))
            xby <- 1 / tsObj$freq
            xx <- seq(xr[1], xr[2], by = xby)
            xd <- as.character(tsObj$data[[timeVar]])

            modlim <- xr
            if (svalue(modLimLower) > 0)
                modlim[1] <- xx[xd == svalue(modLimLower)]
            if (svalue(modLimUpper) > 0)
                modlim[2] <- xx[xd == svalue(modLimUpper)]

            ## if upper limit gets too low, disable lower slider
            if (modlim[2] <= min(xx) + 2) {
                enabled(modLimLower) <<- FALSE
            } else {
                enabled(modLimLower) <<- TRUE
                blockHandlers(modLimLower)
                modLimLower$set_items(xd[xx <= modlim[2] - 2])
                modLimLower$set_value(xd[xx == modlim[1]])
                unblockHandlers(modLimLower)
            }

            ## if lower limit gets too high, disable upper slider
            if (modlim[1] >= max(xx) - 2) {
                enabled(modLimUpper) <<- FALSE
            } else {
                enabled(modLimUpper) <<- TRUE
                blockHandlers(modLimUpper)
                modLimUpper$set_items(xd[xx >= modlim[1] + 2])
                modLimUpper$set_value(xd[xx == modlim[2]])
                unblockHandlers(modLimUpper)
            }

            visible(modLimLower) <<- visible(modLimUpper) <<- TRUE
            if (react) updatePlot()
        },

        ## draw the plot, depending on the settings
        updatePlot = function(animate = FALSE) {
            ## plot the TS object setup by the GUI

            if (animate) gmessage("Animation not yet implemented :(")
            animate <- FALSE

            decomp <<- NULL
            forecasts <<- NULL

            can.smooth <- TRUE
            smooth.t <- smoothness

            updateLimits(react = FALSE)
            updateModLimits(react = FALSE)

            xr <- range(time(tsObj$tsObj))
            xby <- 1 / tsObj$freq
            xx <- seq(xr[1], xr[2], by = xby)
            xd <- as.character(tsObj$data[[timeVar]])

            xlim <- xr
            if (svalue(xlimLower) > 0)
                xlim[1] <- xx[xd == svalue(xlimLower)]
            if (svalue(xlimUpper) > 0)
                xlim[2] <- xx[xd == svalue(xlimUpper)]

            modlim <- xlim
            if (!svalue(modLimEqual)) {
                if (svalue(modLimLower) > 0)
                    modlim[1] <- xx[xd == svalue(modLimLower)]
                if (svalue(modLimUpper) > 0)
                    modlim[2] <- xx[xd == svalue(modLimUpper)]
            }

            visible(forecastError) <<- FALSE

            if (is.null(tsObj)) {
                cat("Nothing to plot ...\n")
                plot.new()
            } else if (inherits(tsObj, "iNZightMTS")) { ## multiple vars
                p <- switch(compare,
                    plot(tsObj,
                        multiplicative = (patternType == 1),
                        xlab = svalue(xLab),
                        ylab = svalue(yLab),
                        t = smooth.t,
                        smoother = show.smoother,
                        xlim = xlim,
                        model.lim = modlim
                    ),
                    plot(tsObj,
                        multiplicative = (patternType == 1),
                        xlab = svalue(xLab),
                        ylab = svalue(yLab),
                        t = smooth.t,
                        smoother = show.smoother,
                        compare=FALSE,
                        xlim = xlim,
                        model.lim = modlim
                    )
                )
            } else { ## single var
                p <- switch(plottype,
                    {
                        ## 1 >> standard plot
                        ## patternType = 1 >> 'multiplicative'; 2 >> 'additive'
                        plot(tsObj,
                            multiplicative = (patternType == 1),
                            ylab = svalue(yLab),
                            xlab = svalue(xLab),
                            animate = animate,
                            t = smooth.t,
                            smoother = show.smoother,
                            xlim = xlim,
                            model.lim = modlim
                        )
                    },
                    {
                        ## 2 >> decomposed plot
                        decomp <<- plot(
                            iNZightTS::decompose(tsObj,
                                t = smooth.t,
                                multiplicative = (patternType == 1),
                                model.lim = modlim
                            ),
                            xlab = svalue(xLab),
                            ylab = svalue(yLab),
                            xlim = xlim,
                            recompose.progress = recompProg
                        )
                        visible(recomposeBtn) <<- TRUE
                        visible(recomposeResBtn) <<- TRUE
                        decomp
                    },
                    {
                        ## 3 >> season plot
                        iNZightTS::seasonplot(tsObj,
                            multiplicative = (patternType == 1),
                            xlab = svalue(xLab),
                            ylab = svalue(yLab),
                            t = smooth.t,
                            model.lim = modlim
                        )
                    },
                    {
                        ## 4 >> forecast plot
                        pl <- try(plot(tsObj,
                            multiplicative = (patternType == 1),
                            xlab = svalue(xLab),
                            ylab = svalue(yLab),
                            xlim = xlim,
                            model.lim = modlim,
                            forecast = tsObj$freq * 2
                        ), silent = TRUE)
                        if (inherits(pl, "try-error")) {
                            visible(forecastError) <<- TRUE
                            return()
                        }
                        forecasts <<- iNZightTS::pred(pl)
                        visible(forecastBtn) <<- TRUE
                        can.smooth <- FALSE
                        pl
                    }
                )

            }
            enabled(smthSlider) <<- can.smooth && show.smoother

            enabled(GUI$plotToolbar$exportplotBtn) <<-
                iNZightPlots::can.interact(p)

            invisible(p)
        # },
        # close = function() {
        #     ## delete the module window
        #     GUI$close_module()
        #     ## display the default view (data, variable, etc.)
        #     GUI$plotToolbar$restore()
        #     GUI$updatePlot()
        }
    )
)
