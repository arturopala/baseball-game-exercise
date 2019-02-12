# Baseball Game Exercise

Conceptual component to study normalization of streamed game events using baseball example.

## Component requirements

- have method to receive game event
- have method to report recent game event
- have method to report all or selected number of game events in reverse order
- try best to normalize inconsistent events

## Game events normalization rules

- reject events where match time is negative 
- reject events where team number is not 0 nor 1
- for first event in the game:
  - if scored points greater than zero then recalculate match score
  - else if match score shows that only one team scored than recalculate points scored and who scored
  - else if match score is (0,0) and no points scored (empty event) then reject event 
  - otherwise leave event as is
- for subsequent events:
  - if scored points gt zero then recalculate match score
  - otherwise calculate delta from previous match score and:
    - if delta shows that none team scored then reject event 
    - else if delta shows that both teams scored then reject event 
    - else if delta shows that only one team scored then recalculate points scored and who scored 
    - otherwise reject event
    
## Build

- Run `sbt compile`
- Test `sbt test`
- Test with coverage `sbt coverage test coverageReport`  