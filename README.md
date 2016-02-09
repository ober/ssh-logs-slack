# ssh-logs-slack
sshd.log tailer that sends to Slack written in Common Lisp.

#### Setup your configuration

1. Update `tail.lisp`
1. Change `*slacktoken` to the token you got from Slack.com
1. Change `*logdir*` to the location of centralized logs. It will recursively find all sshd.log files and tail them.
1. Update `*ssh-log-channel*` to the channel where you want the ssh logs to go.
1. Update `*ssh-slack-user*` to name the bot that shows up in slack.

#### To build.
1. `make ssh-sbcl`
1. Then check `dist/sbcl/ssh-sbcl`

#### Issues:
1. I've used this on `sbcl`, `lispworks` and `ccl` for weeks on end with a hundred logs.
1. It does eat a lot of cpu initially until it reaches the lines that match the last hour.
1. No state kept between reruns.

#### TODO.
1. Keep track of last position in each file on restart.
1. Seek to end to avoid parsing the full file.
1. ...
