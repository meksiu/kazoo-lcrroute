# kazoo-lcrroute (Cloudbased LCR-Route for carriers)
![lcrrates](https://raw.githubusercontent.com/urueedi/kazoo-lcrroute/master/screenshots/lcrroute_1.png)
![lcrrates](https://raw.githubusercontent.com/urueedi/kazoo-lcrroute/master/screenshots/lcrroute_2.png)

Howto Install
=============

- Install kazoo from source
- Install lcrroute in there

help in /doc

```
Candidates:
                           RATE NAME |      COST |                           CARRIERID | INCREMENT |   MINIMUM | SURCHARGE |    WEIGHT |    PREFIX | DIRECTION |
               Switzerland Mobile SR |     0.086 |    5ce3608fd7c547a529ac92efe8877987 |         1 |         1 |         0 |        10 |      4176 |  outbound |
               Switzerland Mobile SR |      0.44 |    72899bb7ec54503175ca8966c602f889 |         1 |         1 |         0 |        10 |      4176 |  outbound |
               Switzerland Mobile SR |      0.22 |    792b841644059354efdcff3e4653414a |        60 |         1 |         0 |        10 |      4176 |  outbound |
                         Switzerland |     0.023 |    5ce3608fd7c547a529ac92efe8877987 |         1 |         1 |         0 |       100 |        41 |  outbound |
                         Switzerland |      0.05 |    792b841644059354efdcff3e4653414a |        60 |         1 |         0 |       100 |        41 |  outbound |
                         Switzerland |      0.00 |    b0697a5becb51fd7301f3e66535d9003 |         1 |         1 |         0 |       100 |        41 |  outbound |
                         Switzerland |     0.009 |    72899bb7ec54503175ca8966c602f889 |         1 |         1 |         0 |       100 |        41 |  outbound |
                              Europe |      0.03 |    5ce3608fd7c547a529ac92efe8877987 |         1 |         1 |         0 |         0 |         4 |  outbound |
Matching:
                           RATE NAME |      COST |                           CARRIERID | INCREMENT |   MINIMUM | SURCHARGE |    WEIGHT |    PREFIX | DIRECTION |
             1 Switzerland Mobile SR |     0.086 |    5ce3608fd7c547a529ac92efe8877987 |         1 |         1 |         0 |        10 |      4176 |  outbound |
               Switzerland Mobile SR |      0.22 |    792b841644059354efdcff3e4653414a |        60 |         1 |         0 |        10 |      4176 |  outbound |
               Switzerland Mobile SR |      0.44 |    72899bb7ec54503175ca8966c602f889 |         1 |         1 |         0 |        10 |      4176 |  outbound |
                         Switzerland |      0.00 |    b0697a5becb51fd7301f3e66535d9003 |         1 |         1 |         0 |       100 |        41 |  outbound |
                         Switzerland |     0.009 |    72899bb7ec54503175ca8966c602f889 |         1 |         1 |         0 |       100 |        41 |  outbound |
                         Switzerland |     0.023 |    5ce3608fd7c547a529ac92efe8877987 |         1 |         1 |         0 |       100 |        41 |  outbound |
                         Switzerland |      0.05 |    792b841644059354efdcff3e4653414a |        60 |         1 |         0 |       100 |        41 |  outbound |
                              Europe |      0.03 |    5ce3608fd7c547a529ac92efe8877987 |         1 |         1 |         0 |         0 |         4 |  outbound |
Carrier sorted resourcelist: 
                        [<<"5ce3608fd7c547a529ac92efe8877987">>,
                         <<"792b841644059354efdcff3e4653414a">>,
                         <<"72899bb7ec54503175ca8966c602f889">>,
                         <<"b0697a5becb51fd7301f3e66535d9003">>]
```