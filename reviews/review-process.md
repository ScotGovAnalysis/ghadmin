# GitHub Organisation Reviews

This document outlines the steps taken in the 2025 membership review. Many of the steps are automated in the R scripts in the `reviews/2025/` folder.

For future reviews, take a copy of the `reviews/2025/` folder, rename for the year of the new review, and make any changes required to the scripts. In particular, update the `review-params` in the `00_setup.R` script.

## Outside Collaborators

### Purpose

There are some GitHub users with access to organisation repositories who are not members of the organisation. There are some legitimate reasons for this, however all Scottish Government employees should be members if using the organisation.

### Tasks

* Run `01_outside-collaborators.R` to identify outside collaborators, their contact details (if available) and which repositories they have access to. Output is saved to the `data` folder.

* Where users can be identified as a Scottish Government employee, by either a public name or email address on their GitHub profile, send an email to request that they join the organisation as a member.

* Where users cannot be identified, contact the owners of the repositories they have access to to help with identification.

* If SG employees do not respond to request to join the organisation by given deadline remove their access (with option to request to join at a later date).

* Record any non-SG employees with a legitimate access need who will continue to be outside collaborators.

## Membership

### Purpose

It is important to periodically review membership to ensure those who don’t need access or have left the SG are removed from the organisation. This is done by asking all existing members to confirm their need for continuing access.

All members need to actively respond to the review to confirm that they need ongoing access. If a member responds to indicate they don’t need access or does not respond at all, they will be removed from the organisation.

### Tasks

* Ensure the `org-admin-all-members` team has write access to the `ghadmin` repository (this repository).

* Review message templates in the `templates` folder. These will be used for issue comments.

* Run `02_review-members.R` to open a membership review issue in the `ghadmin` repository for each organisation member.

* Run `03_close-completed.R` periodically to monitor progress. When all checkboxes are complete, the issue will be closed and a confirmation message posted.

* Run `04_reminders.R` to post a reminder message to those who haven't responded.

* Run `05_remove-members.R` to remove organisation membership for those who haven't responded, close remaining issues and post a confirmation message.

* Save the resulting xlsx file (e.g. `data/2025_github-member-review.xlsx`) to eRDM to keep a record of the process.

* Run `06_review-emails.R` to check that all remaining members have their SG email address public on their profile. This is a requirement for any new member requests and so we should ensure all existing members also meet this requirement. If able to identify individuals from usernames, contact via email to ask them to make their email public ([guidance is available in the welcome repo](welcome/make-email-public.md%20at%20main%20·%20ScotGovAnalysis/welcome)).

## Optional additional checks

The following checks were not carried out during the 2025 review, but may be worth considering in the future.

### Repositories

The organisation repositories should be reviewed to identify:

* Repositories that have been inactive for a long period of time

* Repositories with no files

* Repositories who have no owners and/or users (besides the organisation admins)

### Teams

The organisation teams should be reviewed to identify:

* Teams that have been inactive for a long period of time

* Teams that have no repository access

* Teams that should be consolidated / nested
