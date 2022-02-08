package com.davidjusto.steambancheck.repository;

import com.davidjusto.steambancheck.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * @author davidramiro
 */
@Repository
public interface UserRepo extends JpaRepository<User, Long> {
    User findUserByTelegramIdEquals(Long id);

    boolean existsByTelegramIdEquals(Long id);
}
