'use strict'

function hexToRgba(hex) {
  if(hex[0] == '#') {
    return hexToRgba(hex.substring(1))
  }
  else {
    const hexes = [
      hex.substring(0, 2),
      hex.substring(2, 4),
      hex.substring(4, 6),
    ]
    const [r, g, b] = hexes.map(h => parseInt(h, 16))
    return `rgba(${r}, ${g}, ${b}, 1)`
  }
}

/**
 * Here's the colors taken from
 * https://github.com/nashamri/spacemacs-theme/blob/master/spacemacs-common.el

| name          | color   |
|---------------+---------|
| act1          | #222226 |
| act2          | #5d4d7a |
| base          | #b2b2b2 |
| base-dim      | #686868 |
| bg1           | #292b2e |
| bg2           | #212026 |
| bg3           | #100a14 |
| bg4           | #0a0814 |
| border        | #5d4d7a |
| cblk          | #cbc1d5 |
| cblk-bg       | #2f2b33 |
| cblk-ln       | #827591 |
| cblk-ln-bg    | #373040 |
| cursor        | #e3dedd |
| const         | #a45bad |
| comment       | #2aa1ae |
| comment-bg    | #292e34 |
| comp          | #c56ec3 |
| err           | #e0211d |
| func          | #bc6ec5 |
| head1         | #4f97d7 |
| head1-bg      | #293239 |
| head2         | #2d9574 |
| head2-bg      | #293235 |
| head3         | #67b11d |
| head3-bg      | #293235 |
| head4         | #b1951d |
| head4-bg      | #32322c |
| highlight     | #444155 |
| highlight-dim | #3b314d |
| keyword       | #4f97d7 |
| lnum          | #44505c |
| mat           | #86dc2f |
| meta          | #9f8766 |
| str           | #2d9574 |
| suc           | #86dc2f |
| ttip          | #9a9aba |
| ttip-sl       | #5e5079 |
| ttip-bg       | #34323e |
| type          | #ce537a |
| var           | #7590db |
| war           | #dc752f |
| aqua          | #2d9574 |
| aqua-bg       | #293235 |
| green         | #67b11d |
| green-bg      | #293235 |
| green-bg-s    | #29422d |
| cyan          | #28def0 |
| red           | #f2241f |
| red-bg        | #3c2a2c |
| red-bg-s      | #512e31 |
| blue          | #4f97d7 |
| blue-bg       | #293239 |
| magenta       | #a31db1 |
| yellow        | #b1951d |
| yellow-bg     | #32322c |
 */

const buttonColor = '#0a0814'

const vars = {
  '--border': '#5d4d7a',
  '--func-color': '#bc6ec5',
  '--keyword-color': '#4f97d7',
  '--text': '#b2b2b2',
  '--text-color': '#b2b2b2',
  '--dim-text': '#686868',
  '--background': '#292b2e',
  '--general-background-color': '#292b2e',
  '--general-background': '#292b2e',
  '--code-background-color': '#100a14',
  '--link-color': '#2d9574',
  '--visited-link-color': '#4f97d7',
  '--visited-link-background-color': '#44505c',
  '--heading-background-color': '#292b2e',
  '--head1-color': '#4f97d7',
  '--head1-background-color': '#293239',
  '--head2-color': '#2d9574',
  '--head2-background-color': '#293235',
  '--head3-color': '#67b11d',
  '--head3-background-color': '#293235',
  '--head4-color': '#b1951d',
  '--head4-background-color': '#32322c',
  '--highlight-dim-color': '#e3dedd',
  '--highlight-dim-background-color': '#3b314d',
  '--highlight-background-color': '#444155',
  '--tab-inactive-color': '#2aa1ae',
  '--tab-inactive-background-color': '#292b2e',
  '--tab-left-inactive-border-radius': '0 0 0.5em 0',
  '--tab-right-inactive-border-radius': '0 0 0 0.5em',
  '--meta-color': '#b1951d',
  // Setting the container's background-color makes it so we get the S shaped
  // border radius on the tabs, because the border-radius on the bottom allows
  // the background-color of the container to leak through.
  '--tab-inactive-sibling-container-background-color': '#5d4d7a',
  '--tab-active-background-color': '#5d4d7a',
  '--tab-active-border-radius': '0.5em 0.5em 0 0',
  '--tab-active-color': '#b2b2b2',
  '--tab-list-background-color': '#292b2e',
  '--table-row-alternate-background-color': '#100a14',
  '--button-background': '#2aa1ae',
  '--button-background-color': '#2aa1ae',
  '--button-color': '#0a0814',
  '--button-color-rgba': hexToRgba(buttonColor),
  '--button-hover-color': buttonColor,
  '--button-hover-background-color': '#6ae1ee',
  '--input-color': '#2aa1ae',
  '--input-background-color': '#292e34',
  '--input-label-color': '#2aa1ae',
  /**
   * Active items are usually the ones important under a context. They will
   * typically be whatever the "selected" thing is.
   */
  '--item-active-color': '#e3dedd',
  '--item-active-background-color': '#5d4d7a',
  '--thin-border': '0.1em solid #5d4d7a',
  '--thin-border-color': '#5d4d7a',
  '--diff-add-color': '#293235',
  '--diff-add-background-color': '#86dc2f',
  '--diff-minor-add-background-color': '#67b11d',
  '--diff-remove-color': '#3c2a2c',
  '--diff-remove-background-color': '#e0616d',
  '--diff-minor-remove-background-color': '#ce537a',
  '--open-ticket-color': '#86dc2f',
  '--closed-ticket-color': '#ce537a',
  '--error-color': '#ce537a',
  '--error-background-color': '#3c2a2c',
  '--calendar-leave-entry-background': '#29422d',
  '--calendar-leave-entry-color': '#67b11d',
  'code-string-interpolation': '#86dc2f',
  '--panel-border': '0.1em solid #5d4d7a',
  '--panel-title-color': '#b2b2b2',
  '--panel-title-background': '#5d4d7a',
  '--panel-title-padding': '1em',
  'panel-background-color': '#0a0814', // Uses "bg4" from color list.
  '--callout-color': '#e3dedd',
  '--callout-background-color': '#5d4d7a',
  // Emacs calls this "war" and it's typically used for callouts on headings
  // such as [1/3] and [50%] that are shown from TODO check lists.
  '--badge-color': '#dc752f',
  '--time-slice': '#2d9574',
  '--time-slice-background': '#29422d',
  '--time-slice-inactive-background': '#100a14',
  '--time-slice-cursor-background': '#5d4d7a',
}

module.exports = vars
