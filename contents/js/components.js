const addEventNavBar = () => {
    document.addEventListener('DOMContentLoaded', () => {
        const navbarBurgers = Array
            .prototype
            .slice.call(document.querySelectorAll('.navbar-burger'), 0);
            
        // navbar
        if (navbarBurgers.length > 0) {
            navbarBurgers.forEach(el => {
                el.addEventListener('click', () => {
                    const target = document.getElementById(el.dataset.target);
                    el.classList.toggle('is-active');
                    target.classList.toggle('is-active');
                });
            });
        }
    });
}

const addEventModal = () => {
    document.addEventListener('DOMContentLoaded', () => {
        const modalTarget = document.getElementById('modal-target');
        const modalCloses = Array
        .prototype
        .slice.call(document.querySelectorAll('.delete, .modal-background'), 0);
    
        // modal
        modalTarget.addEventListener('click', () => {
            document
                .getElementById(modalTarget.dataset.target)
                .classList
                .add('is-active');
        });
        if (modalCloses.length > 0) {
            modalCloses.forEach(el => {
                el.addEventListener('click', () => { 
                    document
                        .getElementById(modalTarget.dataset.target)
                        .classList
                        .remove('is-active');
                });
            })
        }
    });
}
