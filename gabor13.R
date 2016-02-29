## -*- coding: utf-8 -*-

## Aktualna wersja obrazu systemu operacyjnego jest tutaj: http://149.156.92.47/image-R

## Aby skopiować obraz systemu na pendrive'a pod Windowsem można użyć
## programu Win32DiskImager, dostępnego tutaj:
## https://sourceforge.net/projects/win32diskimager/
##
## UWAGA wybranie złej litery dysku docelowego może grozić utratą
## danych

## TODO wielkość czcionki w oknie z instrukcją, w ogóle wygląd okna z instrukcją

if(interactive())source('~/cs/code/r/tasks/task/task.R')

## Globalne parametry zadania

FIXATION.TIME = 1000
POST.STIM.TIME = 0
LONELY.MASK.DURATION = 0
SCALE.MAX.DURATION = 4000
MAX.REACTION.TIME = 4000
FEEDBACK.TIME = 1000
block.length = 24

## Parametry rysowania gabora i skali

sigma = .01
f = 20
contrast = .3
scale.position = .75

## Globalne obiekty graficzne

TXT$set.string("Proszę nacisnąć spację")
center(TXT, WINDOW)
FX = fixation(WINDOW, size = .02)
STIM = new(Text)
STIM$set.font(FONT)

i = new(Image)
mask = new(Image)
## Tworzymy tylko tyle obrazka, ile potrzeba
for(obj in c(i, mask)){
    obj$create(WINDOW$get.size()[1] * 6 * sigma, WINDOW$get.size()[1] * 6 * sigma, c(0, 0, 0))
}
draw.sin(i, f = f, 45, sigma = sigma, contrast = contrast, mask = F)
draw.sin(mask, f = f, 45, sigma = sigma, mask = T)
i.texture = new(Texture)
i.texture$create(i$size[1], i$size[2])
i.texture$update(i, 0, 0)
mask.texture = new(Texture)
mask.texture$create(mask$size[1], mask$size[2])
mask.texture$update(mask, 0, 0)
s = new(Sprite)
s$set.texture(i.texture, F)
center(s, WINDOW)
m = new(Sprite)
m$set.texture(mask.texture, F)
center(m, WINDOW)

## Funkcje pomocnicze, typu rysowanie bodźców

draw.stim = function(side){
    if(side == 'left'){
        s$set.rotation(-90)
    }else{
        s$set.rotation(0)
    }
    WINDOW$draw(s)
}

## Dwa klawisze w kluczu reakcyjnym

KEYS <<- c(Key.Left, Key.Right)

trial.code = function(trial, side = 'left', decorder = 'type1', duration = 1000, withscale = 1, feedback = 0){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        state = 'press-space'
    }else if((trial %% block.length) == 0){
        state = 'break'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("Naciśnij spację aby rozpocząć zadanie")
            WINDOW$draw(center.win(TXT))
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'break' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("Krótka przerwa - odpocznij. Aby kontynuować, naciśnij spację")
            WINDOW$draw(center.win(TXT))
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(.5, .5, .5))
            ## Losowa pozycja myszki
            mouse.set.position(c(runif(1), .5) * WINDOW$get.size())
            ## Punkt fiksacji
            lapply(FX, WINDOW$draw)
            WINDOW$set.mouse.cursor.visible(F)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$set.mouse.cursor.visible(F)
                WINDOW$clear(c(.5, .5, .5))
                WINDOW$display()
                state = 'show-gabor'
                fixation.cleared = CLOCK$time
            }
        }, 'show-gabor' = {
            WINDOW$clear(c(.5, .5, .5))
            draw.stim(side)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'gabor-present'
        }, 'gabor-present' = {
            if((CLOCK$time - stim.onset) > duration){
                ## Znikamy gabora
                WINDOW$clear(c(.5, .5, .5))
                WINDOW$display()
                stim.cleared = CLOCK$time
                state = 'post-gabor'
            }
        }, 'post-gabor' = {
            if((CLOCK$time - stim.cleared) > POST.STIM.TIME){
                WINDOW$draw(m)
                WINDOW$display()
                mask.onset = CLOCK$time
                scale.rt = scale.value = -1
                mp = 666
                state = 'mask-present'
            }
        }, 'mask-present' = {
            if((CLOCK$time - mask.onset) > LONELY.MASK.DURATION){
                if((decorder == 'type2') && (withscale == 1)){
                    scale.onset = CLOCK$time
                    state = 'draw-scale'
                }else{
                    state = 'show-leftright'
                }
            }
        }, 'draw-scale' = {
            if(((CLOCK$time - scale.onset) > SCALE.MAX.DURATION) ||
               (BUTTON.PRESSED[1] > scale.onset)){
                    scale.rt = BUTTON.PRESSED[1] - scale.onset
                    scale.value = mp[1]
                    if(decorder == 'type2'){
                        state = 'show-leftright'
                    }else{
                        state = 'done'
                    }
            }else{
                WINDOW$clear(c(.5, .5, .5))
                WINDOW$draw(m)
                mp = draw.scale(list(M = c('Nic nie widziałem', 'Widziałem bardzo wyraźnie'),
                                     K = c('Nic nie widziałam', 'Widziałam bardzo wyraźnie'))[[USER.DATA$gender]],
                                     background.color = c(.5, .5, .5), position = scale.position)
                WINDOW$display()
            }
        }, 'show-leftright' = {
            WINDOW$clear(c(.5, .5, .5))
            WINDOW$draw(m)
            TXT$set.string("LEWO     PRAWO")
            center(TXT, WINDOW)
            TXT$set.position(c(WINDOW$get.size()[1] / 2, WINDOW$get.size()[2] * scale.position))
            WINDOW$draw(TXT)
            WINDOW$display()
            leftright.onset = CLOCK$time
            CORRECT.KEY <<- c(left = Key.Left, right = Key.Right)[side]
            ACC <<- RT <<- NULL
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(!is.null(ACC) || ((CLOCK$time - leftright.onset) > MAX.REACTION.TIME)){
                if((decorder == 'type1') && (withscale == 1)){
                    scale.onset = CLOCK$time
                    state = 'draw-scale'
                }else{
                    if(feedback == 1){
                        feedback.onset = CLOCK$time
                        state = 'feedback'
                    }else{
                        state = 'done'
                    }
                }
            }
        }, 'feedback' = {
            if((CLOCK$time - feedback.onset) < FEEDBACK.TIME){
                WINDOW$clear(c(.5, .5, .5))
                TXT$set.string(c('Źle', 'Dobrze', 'Za późno')[ifelse(is.null(ACC), 3, ACC + 1)])
                WINDOW$draw(center.win(TXT))
                WINDOW$display()
            }else{
                state = 'done'
            }
        }, 'done' = {
            WINDOW$clear(c(.5, .5, .5))
            WINDOW$display()
            return(list(scalert = scale.rt, scalevalue = scale.value,
                        rt = ifelse(is.null(RT), MAX.REACTION.TIME, RT - stim.onset),
                        acc = ifelse(is.null(ACC), 2, ACC)))
        })
    }
}

TASK.NAME <<- 'gabor13'

gui.show.instruction("W czasie eksperymentu obowiązuje cisza. Wyłącz telefon komórkowy. W razie jakichkolwiek wątpliwości nie wołaj osoby prowadzącej, tylko podnieś do góry rękę - osoba prowadząca podejdzie w dogodnym momencie i postara się udzielić wszelkich wyjaśnień. 
Badanie jest anonimowe. Za chwilę zostaniesz poproszona/y o podanie danych: wieku, płci oraz pseudonimu. Pseudonim składa się z inicjałów oraz czterech cyfr: dnia 
i miesiąca urodzenia (np.  ms0706). 
")
gui.user.data()
cnd = source.random.condition()

## Trening1: 16 prób, czas prezentacji 512, feedback, bez skali
gui.show.instruction(list(K = "Badanie dotyczy percepcji oraz świadomości wzrokowej.
Twoim głównym zadaniem będzie decydowanie, czy czarno-białe paski pojawiające się na ekranie są pochylone w lewą czy w prawą stronę. Paski będą czasami prezentowane bardzo krótko, jeśli nie będziesz wiedzieć, w którą stronę są pochylone, po prostu zgaduj. 
Twoim drugim zadaniem będzie odpowiedź na pytanie jak dobrze widziałaś wzroki. Będziesz zaznaczać ją na skali opisanej od „nic nie widziałam” do „widziałam bardzo wyraźnie”.
W trakcie trwania wszystkich zadania staraj się zachować skupienie oraz nie przysuwać się w kierunku ekranu.Teraz będzie się pierwszy trening, żebyś zobaczyła jak wyglądają czarno-białe paski.
Na początku każdej „próby” na środku ekranu pojawi się krzyżyk. Staraj się koncentrować na nim swój wzrok. Po zniknięciu krzyżyka na ekranie pojawią się czarno-białe paski, które następnie zostaną przesłonięte czarno-białą szachownicą. Gdy pojawią się opcje „prawo – lewo” wciśnij strzałkę w lewo, jeśli paski były pochylone w lewo, a strzałkę w prawo, jeśli paski były pochylone w prawo.",
                          M = "Badanie dotyczy percepcji oraz świadomości wzrokowej.
Twoim głównym zadaniem będzie decydowanie, czy czarno-białe paski pojawiające się na ekranie są pochylone w lewą czy w prawą stronę. Paski będą czasami prezentowane bardzo krótko, jeśli nie będziesz wiedzieć, w którą stronę są pochylone, po prostu zgaduj. 
Twoim drugim zadaniem będzie odpowiedź na pytanie jak dobrze widziałeś wzroki. Będziesz zaznaczać ją na skali opisanej od „nic nie widziałem” do „widziałem bardzo wyraźnie”.
W trakcie trwania wszystkich zadania staraj się zachować skupienie oraz nie przysuwać się w kierunku ekranu.
Teraz będzie się pierwszy trening, żebyś zobaczył jak wyglądają czarno-białe paski.
Na początku każdej „próby” na środku ekranu pojawi się krzyżyk. Staraj się koncentrować na nim swój wzrok. Po zniknięciu krzyżyka na ekranie pojawią się czarno-białe paski, które następnie zostaną przesłonięte czarno-białą szachownicą. Gdy pojawią się opcje „prawo – lewo” wciśnij strzałkę w lewo, jeśli paski były pochylone w lewo, a strzałkę w prawo, jeśli paski były pochylone w prawo.")[[USER.DATA$gender]])
run.trials(trial.code, condition = cnd, expand.grid(side = c('left', 'right'),
                                                    decorder = 'type1', withscale = 0, feedback = 1,
                                                    duration = 512), b = 8)

## Trening2: 12 prób, czas prezentacji 128, feedback, bez skali
gui.show.instruction("Teraz będzie drugi trening, w którym paski będą wyświetlane krócej.")
run.trials(trial.code, condition = cnd, expand.grid(side = c('left', 'right'),
                                                    decorder = 'type1', withscale = 0, feedback = 1,
                                                    duration = 128), b = 6)

## Trening3: 12 prób, czas prezentacji 128, bez feedkacku, skala
gui.show.instruction(INSTR)
run.trials(trial.code, condition = cnd, expand.grid(side = c('left', 'right'),
                                                    decorder = ORDER, withscale = 1, feedback = 0,
                                                    duration = c(16, 128, 32, 32, 64, 64)), b = 1)

## Etap właściwy
gui.show.instruction('Teraz zacznie się właściwe zadanie, które będzie wyglądać dokładnie tak samo jak trzeci trening. Będzie przedzielone przerwami. Pamiętaj, aby utrzymać skupienie i nie przybliżać ani nie oddalać się od ekranu.')
run.trials(trial.code, condition = cnd, record.session = T,
           expand.grid(side = c('left', 'right'),
                       decorder = ORDER, withscale = 1, feedback = 0,
                       duration = c(16, 128, 32, 32, 64, 64)), b = 12)
if(!interactive())quit("no")
