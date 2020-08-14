const TABS = [...document.querySelectorAll("#tabs li")];
const CONTENT = [...document.querySelectorAll("#tab-content div")];
const ACTIVE_CLASS = "is-active";

const update_active_tab = (selected) => {
    TABS.forEach(function(tab) {
        if (tab && tab.classList.contains(ACTIVE_CLASS)) {
            tab.classList.remove(ACTIVE_CLASS);
        }
    });
    selected.classList.add(ACTIVE_CLASS);
}

const update_active_content = (selected) => {
    CONTENT.forEach(function(item) {
        if (item && item.classList.contains(ACTIVE_CLASS)) {
            item.classList.remove(ACTIVE_CLASS);
        }
        let data = item.getAttribute("data-content");
        if (data === selected) {
            item.classList.add(ACTIVE_CLASS);
        }
    });
}

const init_tabs = () => {
    TABS.forEach((tab) => {
      tab.addEventListener("click", (e) => {
        let selected = tab.getAttribute("data-tab");
        update_active_tab(tab);
        update_active_content(selected);
      })
    })
}

init_tabs();
